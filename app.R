
library(shiny)
library(httr)
library(jsonlite)
library(tidyverse)
library(janitor)
library(purrr)
library(furrr)
library(formattable)
library(htmlwidgets)

options(stringsAsFactors = FALSE)

u_namepairs_df <- read_csv("unit_base_to_name_chardefinitionid.csv",
                           show_col_types = FALSE)
statname_df <- read_csv("statname.csv",
                        show_col_types = FALSE)
modset_stat <- read_rds("modset_stat.rds")
modslot_shape <- read_rds("modslot_shape.rds")
calibrate_cost_df <- read_rds("calibrate_cost_df.rds")
prim_stat_name <- read_rds("prim_stat_name.rds")

whitelist <- read_csv("unleashed_whitelist.csv")


# metares <- POST("http://localhost:3200/metadata")
# metacontent <- metares$content %>% rawToChar() %>% fromJSON()
# meta_gameversion <- metacontent$latestGamedataVersion
# rm(metares)
# rm(metacontent)


get.fresh.pdata <- function(a){
    ally <- as.character(a)
    
    
    ## Fetch data
    paylist <- list(payload = 
                        list(allyCode = ally),
                    #enums = TRUE)
                    enums = FALSE) %>% 
        toJSON(pretty = TRUE, auto_unbox = TRUE)
    pres <- POST(
        #"http://localhost:3200/player",
        "https://gasapi.info/comlink/player",
        content_type_json(),
        body = paylist)
    pres$status_code
    pcontent <- pres$content %>% rawToChar() %>% fromJSON()
    return(pcontent)
}

process_caltargets <- function(pd, speed_weighting = 10){
    pcontent <- pd
    sweight <- speed_weighting/100
    ## Process data
    char_ids <- 
        pcontent$rosterUnit %>% 
        select(char_id = id, 
               char_definitionId = definitionId, 
               equippedStatMod) %>% 
        # only keep characters with mods
        filter(length(equippedStatMod) > 0) %>% 
        unnest(equippedStatMod)
    
    has.any.sec.stats <- function(x){
        x %>% length() > 0 %>% return()
    }
    
    has.sec.spd <- function(x){
        # Identify Mods with speed secondaries
        x %>% filter(stat$unitStatId == 5) %>% nrow() > 0 %>% return()
    }
    get.sec.spd.rolls <- function(x){
        # Fetch rolls for speed
        ## Might be unnecessary
        x %>% filter(stat$unitStatId == 5) %>% 
            pull(roll)
    }
    get.sec.spd.unscalled <- function(x){
        # Fetch unscalledRolls for speed
        x %>% filter(stat$unitStatId == 5) %>% 
            pull(unscaledRollValue)
    }
    sum.sec.spd.unscalled <- function(x){
        # Get current speed - does not adjust for 6-dot
        x[[1]] %>% as.numeric() %>% sum()
    }
    get.highest.nonspeed.roll <- function(x){
        # Finds the non-speed stat with most rolls, returns "stat_num|roll_n"
        y <- x %>% filter(stat$unitStatId != 5) %>% 
            unnest(roll) %>% 
            count(stat_id = stat$unitStatId) %>% 
            arrange(desc(n)) %>% 
            .[1,]
        res <- paste0(y$stat_id, "|", y$n)
    }
    weighted.spd <- function(n, g){
        # Making every speed increment 10% more valuable than the last
        res <- 0
        for(i in 1:g){
            res <- (1+sweight)^(n+i) + res
        }
        return(res)
    }
    
    spd.probs <- function(speed_dbl, chance_to_hit_speed, cost){
        newmin <- floor(speed_dbl + 3)
        newmax <- newmin + 3
        
        data.frame(newspd = c(seq(newmin, newmax, 1))) %>% 
            mutate(gain = newspd - floor(speed_dbl)) %>% 
            mutate(minq = pmax(((gain - (speed_dbl %% floor(speed_dbl)) - 3) / 3),0)) %>% 
            mutate(prob = lead(minq, 1, default = 1) - minq) %>% 
            mutate(abs_prob = prob * chance_to_hit_speed) %>% 
            rowwise() %>% 
            mutate(weighted_speed_value =
                       weighted.spd(floor(speed_dbl), gain)) %>% 
            ungroup() %>% 
            mutate(exp_spd_partial = gain * abs_prob) %>% 
            mutate(exp_weighted_spd_partial = weighted_speed_value * abs_prob) %>% 
            summarize(
                exp_spd = sum(exp_spd_partial), 
                exp_weighted_spd = sum(exp_weighted_spd_partial)) %>% 
            mutate(exp_spd_percost = exp_spd / cost) %>% 
            mutate(exp_weighted_spd_percost = exp_weighted_spd / cost)
        # %>% 
        #   list()
    }
    
    plan(multisession, workers = 4)
    
    df_speed <- char_ids %>%
        mutate(rarity = substr(definitionId, 2, 2) %>% 
                   as.integer()) %>% 
        #filter(rarity == 6) %>% 
        mutate(has_secondaries = future_map_lgl(secondaryStat, ~ has.any.sec.stats(.x))) %>% 
        filter(has_secondaries == TRUE) %>% 
        mutate(hasspd = future_map_lgl(secondaryStat, ~ has.sec.spd(.x))) %>%
        filter(hasspd == TRUE) %>%
        mutate(spd_unscalled = future_map(secondaryStat, ~get.sec.spd.unscalled(.x))) %>%
        mutate(spd_roll_n = future_map_dbl(spd_unscalled, function(x) length(x[[1]]))) %>%
        filter(spd_roll_n < 5) %>%
        mutate(sum_unscalled = future_map_dbl(spd_unscalled, ~sum.sec.spd.unscalled(.x))) %>%
        # get mod data
        mutate(setId = substr(definitionId, 1,1)) %>% 
        left_join(modset_stat %>% select(setId, set = short_name), by = "setId") %>% 
        mutate(last_digit_definitionid = 
                   substr(definitionId, 3, 3) %>% as.numeric()) %>% 
        left_join(modslot_shape %>% select(last_digit_definitionid, shape),
                  by = "last_digit_definitionid") %>% 
        unnest(primaryStat) %>% 
        unnest(stat) %>% 
        left_join(statname_df %>% 
                      select(unitStatId = id, primary_stat = name),
                  by = "unitStatId") %>% 
        left_join(prim_stat_name %>% select(primary_stat, primary = short_name), by = "primary_stat") %>% 
        left_join(u_namepairs_df, by = "char_definitionId") %>% 
        mutate(speed_dbl = sum_unscalled/100000) %>% 
        mutate(speed_dbl = case_when(
            rarity == 6 ~ speed_dbl + 1,
            TRUE ~ speed_dbl
        )) %>% 
        mutate(speed_int = floor(speed_dbl)) %>% 
        mutate(speed_remainder = speed_dbl - speed_int) %>% 
        mutate(highest_nonspeed = future_map_chr(secondaryStat, ~get.highest.nonspeed.roll(.x))) %>% 
        mutate(highest_nonspeed_stat = str_extract(highest_nonspeed, "^[0-9]+")) %>% 
        mutate(highest_nonspeed_roll = str_extract(highest_nonspeed, "[0-9]+$") %>% as.integer()) %>% 
        mutate(has5rollstat = highest_nonspeed_roll == 5) %>% 
        select(name, shape, rarity, tier, set, primary_stat, primary, spd_roll_n, 
               spd_unscalled, sum_unscalled, speed_dbl, speed_int, 
               speed_remainder, rerolledCount, highest_nonspeed, 
               highest_nonspeed_stat, highest_nonspeed_roll, has5rollstat) %>% 
        left_join(calibrate_cost_df, by = "rerolledCount") %>% 
        mutate(chance_to_hit_speed = 
                   case_when(
                       has5rollstat == TRUE ~ 1/3,
                       has5rollstat == FALSE ~ 1/4))
    
    final_df <- df_speed %>% 
        select(speed_dbl, chance_to_hit_speed, cost) %>% 
        future_pmap_dfr(spd.probs) %>% 
        bind_cols(df_speed, .)
    # toc()
    # print(paste0("^^^", w, " workers^^^"))
    # }
    plan(sequential)
    
    pname <- paste0(pcontent$name, "'s")
    
    df_sorted <- final_df %>% 
        mutate(Speed = paste0("<b>",speed_int, "</b>(", spd_roll_n, ")")) %>% 
        mutate(chance_to_hit_speed = (chance_to_hit_speed * 100) %>% floor() %>% paste0("%")) %>% 
        mutate(exp_spd = round(exp_spd, 2)) %>% 
        mutate(exp_spd_percost = round(exp_spd_percost, 3)) %>% 
        mutate(exp_weighted_spd = round(exp_weighted_spd, 2)) %>% 
        ## move lower so the full precision is present for the sort
        #mutate(exp_weighted_spd_percost = round(exp_weighted_spd_percost, 2)) %>% 
        mutate(tier = 
                   case_when(
                       tier == 1 ~ "E",
                       tier == 2 ~ "D",
                       tier == 3 ~ "C",
                       tier == 4 ~ "B",
                       tier == 5 ~ "A"
                   )) %>% 
        mutate(Mod = paste0(rarity, "-", tier, " ", shape, " (", set, ":", primary,")")) %>% 
        arrange(desc(exp_weighted_spd_percost)) %>% 
        mutate(exp_weighted_spd_percost = round(exp_weighted_spd_percost, 2)) %>% 
        mutate(`#` = row_number()) %>% 
        select(
            `#`,
            `Mod (Set:Primary)` = Mod,
            #Shape = shape, 
            #Character = name, 
            !! pname := name,
            Speed, 
            `Speed Remainder` = speed_remainder, 
            Cost = cost, 
            `Speed Hit %` = chance_to_hit_speed, 
            `Avg +Speed` = exp_spd,
            `Speed per cost` = exp_spd_percost,
            `Weighted +Speed` = exp_weighted_spd, 
            `Weighted per cost` = exp_weighted_spd_percost)
    
    return(df_sorted)
    
}

customGreen0 = "#DeF7E9"
customGreen = "#71CA97"
customRed = "#ff7f7f"

ftable_settings <- list(
    `Speed Hit %` =
        formatter("span",
                  style = ~style(color = ifelse(`Speed Hit %` == "33%", "black", "#D23208"))),
    `Speed Remainder`=
        color_bar(customRed, fun = function(x) x),
    `Avg +Speed`=
        color_tile("white", customGreen),
    `Cost`=
        color_tile("#ffffff", customRed),
    `Speed per cost`=
        color_tile("white", customGreen),
    `Weighted +Speed`=
        color_tile("white", customGreen),
    `Weighted per cost`=
        color_tile("white", customGreen)
)

col_sort_options <- c(
    "Speed Remainder",
    "Cost",
    "Speed Hit %",
    "Avg +Speed",
    "Speed per cost",
    "Weighted +Speed",
    "Weighted per cost")

ui <- fluidPage(
    
    # Application title
    titlePanel("Unleashed Mod Calibration Candidates - Closed Beta"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            numericInput("acode",
                         "Ally Code:",
                         min = 111111111,
                         max = 999999999,
                         #value = 491988799,
                         #value = 587792678,
                         value = NULL,
                         width = '50%'
            ),
            actionButton(inputId = "pfetch_button",
                         label = "Refresh Mod Data"),
            "(can take a minute)"
            # actionButton(inputId = "process_mods_button",
            #              label = "Analyze Mod Data (slow)")
        ),
        
        mainPanel(
            textOutput(outputId = "feedback1"),
            textOutput(outputId = "pname"))
    ),
    verticalLayout(
        flowLayout(
            sliderInput(inputId = "numModsToShow",
                        label = "Number of Mods to Show",
                        min = 10,
                        max = 100,
                        step = 10,
                        value = 10,
                        ticks = FALSE,
                        width = '300px'),
            selectizeInput(inputId = "sortColumnChoice",
                           label = "Sort By",
                           choices = col_sort_options,
                           selected = "Weighted per cost",
                           multiple = FALSE),
            checkboxInput(inputId = "sortChoice",
                          label = "Sort Descending",
                          value = TRUE),
            # checkboxGroupInput(inputId = "dotsShowChoice",
            #                    label = "",
            #                    choices = c("5-dot", "6-dot"),
            #                    selected = c("5-dot", "6-dot")),
            checkboxGroupInput(inputId = "m5dotShowChoice",
                               label = "5-dot",
                               choices = c("E" = "5-E",
                                           "D" = "5-D",
                                           "C" = "5-C",
                                           "B" = "5-B",
                                           "A" = "5-A"),
                               selected = c("5-E","5-D","5-C","5-B","5-A")),
            checkboxGroupInput(inputId = "m6dotShowChoice",
                               label = "6-dot",
                               choices = c("E" = "6-E",
                                           "D" = "6-D",
                                           "C" = "6-C",
                                           "B" = "6-B",
                                           "A" = "6-A"),
                               selected = c("6-E","6-D","6-C","6-B","6-A")),
            checkboxGroupInput(inputId = "spdRollsToShow",
                               label = "Speed rolls",
                               choices = c(1,2,3,4),
                               selected = c(1,2,3,4))
        ),
        # verbatimTextOutput(outputId = "feedback2"),
        formattableOutput("table")),
    p(strong("Speed Remainder"), " - Fractional speed remaining on the mod that will add to the next roll"),
    p(strong("Cost"), " - Number of Attenuators required to calibrate"),
    p(strong("Speed Hit %"), " - Chance that a calibration will give stats to speed (25% normally, 33% if
another stat has 5-rolls)"),
    p(strong("Avg +Speed"), " - Expected speed gain from any one calibration (combines Speed Remainder and Speed Hit %"),
    p(strong("Speed per Cost"), " - Avg +Speed divided by Cost"),
    p(strong("Weighted +Speed"), " - Expected speed gain, with each speed increment given additional value based on
the final total speed (will prioritize higher speed mods)"),
    p(strong("Weighted per cost"), " - Weighted +Speed divided by Cost. I consider this the best single metric
for a calibration candidate, though the player should consider overall mod synergy, depth, and other factors.")
    
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    pdata_react <- reactiveValues(datapull = list())
    
    observeEvent(input$pfetch_button, {
        # pdata <- reactive({
        #     pdatatemp <- get.fresh.pdata(input$acode) 
        #     return(pdatatemp)
        # pdata <- get.fresh.pdata(input$acode)
        pulled_acode <- input$acode
        
        if(!pulled_acode %in% whitelist$ally_code){
            output$feedback1 <- renderText(paste0("Allycode ", pulled_acode, " is not whitelisted for this closed beta. I hope to release this more widely soon"))
        } else {
            
            # if(pulled_acode %in% whitelist$ally_code){
            
            # output$feedback1 <- renderText(paste0("Pulling data for ", pulled_acode))
            pdata_react$datapull <- get.fresh.pdata(input$acode)
            
            # output$feedback1 <- renderText(paste0("Pulled ", 
            #                                       pdata_react$datapull %>% .$rosterUnit %>% .$equippedStatMod %>% length(),
            #                                       " units for allycode ", pulled_acode, "\n", "Processing... (might take a minute)"))
            
            pdata_react$processed <- process_caltargets(pdata_react$datapull) 
            
            
            output$feedback1 <- renderText(paste0("Pulled ", 
                                                  pdata_react$datapull %>% .$rosterUnit %>% .$equippedStatMod %>% length(),
                                                  " units for allycode ", pulled_acode, " --- ", "Processing Complete!"))
            
            # output$feedback2 <- renderPrint(head(pdata_react$processed))
        }
        # } else {break}
        
        
    })
    
    output$pname <- renderText(pdata_react$datapull$name)
    
    # observeEvent(input$process_mods_button, {
    #     pdata_react$processed <- process_caltargets(pdata_react$datapull) 
    #     
    #     output$feedback2 <- renderPrint(head(pdata_react$processed))
    # })
    
    # observeEvent(input$dotsShowChoice){
    #     if(!"5-dot" %in% input$dotsShowChoice){
    #         pdata_react$display <- 
    #             pdata_react$processed %>% 
    #             filter(!str_detect(`Mod (Set:Primary)`, "^5"))
    #     }
    # }
    
    observeEvent(pdata_react$processed, ignoreNULL = TRUE, {
        output$table <- renderFormattable(
            formattable({
                # Table filter/arrange
                pdata_react$processed %>%
                    filter(str_sub(`Mod (Set:Primary)`, 1, 3) %in% input$m5dotShowChoice |
                               str_sub(`Mod (Set:Primary)`, 1, 3) %in% input$m6dotShowChoice) %>% 
                    # 5 or 6 dot mods
                    # filter(
                    #     ("5-dot" %in% input$dotsShowChoice & str_detect(`Mod (Set:Primary)`, "^5")) |
                    #         ("6-dot" %in% input$dotsShowChoice & str_detect(`Mod (Set:Primary)`, "^6"))) %>% 
                    filter(str_sub(Speed, -2,-2) %in% input$spdRollsToShow) %>% 
                    
                    # Arrange by column choice, sort changed by checkbox
                    arrange(-sign(input$sortChoice - 0.5)* (!! rlang::sym(input$sortColumnChoice))) %>% 
                    .[1:input$numModsToShow,]
            },
            align = c("r","l","l","l","r","r","r","r","r","r","r"),
            ftable_settings)
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
