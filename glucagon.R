if(!require(pacman)) install.packages("pacman"); pacman::p_load(ggplot2, data.table, gtable, ggpubr, patchwork, ggstats, ggrepel, rvest)

years <- 2012:2023
NDC <- c("00002614501", "00002614502", "00002614511", "00002614527", "00002803101",
         "00169706515", "00597005301", "00597005345", "00597026010", "54569473400",
         "63323058282", "63323059303", "63323059403", "63323059603", "63323059606",
         "63323059608", "63323059611", "63323059613", "63323059616", "72065012011",
         "72065012012", "72065012111", "72065012112", "72065013011", "72065013012",
         "72065013111", "72065013112", "51662132301", "51662149801", "00548585000",
         "72065013199", "00169706521", "55390000401", "55390000410", "00002808501",
         "72065014011", "72065014111", "80644001201", "80644001202", "80644001301",
         "80644001302", "72065013099", "00002752900", "00169191201", "00169191202",
         "00169191301", "00169191302", "11014039101", "41524000501", "43742025201",
         "43742073401", "43858071000", "50090063901", "54868507000", "55463000900",
         "55463000901", "57520090901", "63083155501", "64725803101", "66558019501",
         "66558019502", "66558019503", "63083155501", "63323018582", "63323058313",
         "63323018503", "00548590500", "00548583100", "00002752901", "00002753001",
         "00597026594", "63323185003", "63323185082", "63323583013")

#Download and Create file if 'output_raw.csv' does not exist in directory=======
#Most recent data pull was Aug 10 2023.
#As of Dec 1 2023, it appears that half of the data from 2022 has been rescinded or removed
#Data were updated to 2023, subbing in the originally pulled data (on Aug 10, 2023) from 2022 as it would appear that ~half of the 2022 data has been lost when querying the medicaid API
if(!file.exists("Cleaned Data/output_raw.csv")){
  if (!dir.exists("Data")) {
    dir.create("Data")
    message("Data Directory Created")
  } else message("Data Directory Found")
  
  for (i in seq_along(years)){
    options(timeout = 180)
    dest_path = paste0("Data/sdud", years[i], ".csv")
    
    if (file.exists(dest_path)) {
      message("File already exists: ", dest_path, ". Skipping download.")
      next
    }
    
    success = FALSE
    repeat {
      download_error = tryCatch({
        download.file(paste0("https://download.medicaid.gov/data/state-drug-utilization-data", years[i], ".csv"),
                      destfile = dest_path)
        success = TRUE
      },
      error = function(x){
        cat("Download failed for ", dest_path, ". Retrying...")
      })
      if (success){
        break
      }
    }
  }

  data_list <- lapply(list.files("Data", full.names = TRUE), function(x){
    DT <- setDT(fread(x, colClasses = list(character = 3)))
    DT <- setnames(DT, names(DT), gsub(" ", "_", tolower(names(DT))))
    
    #2023 Fixes ================================================================
    names_pre23 <- c("utilization_type", "state", "ndc", "labeler_code", "product_code", "package_size", "year", "quarter",
                     "suppression_used", "product_name", "units_reimbursed", "number_of_prescriptions" , "total_amount_reimbursed", 
                     "medicaid_amount_reimbursed", "non_medicaid_amount_reimbursed")
    
    names_23 <- c("record_id", "state_code", "ndc", "labeler_code", "product_code", "package_size", "year", "quarter", 
                  "supression_used", "product_fda_list_name", "units_reimbursed", "no._of_prescriptions", "total_amount_reimbursed",
                  "medicaid_amount_reimbursed", "non-medicaid_amount_reimbursed")
    if(x == "Data/sdud2023.csv") setnames(DT, names_23, names_pre23)
    
    out <- DT[state == "XX" &
                !is.na(units_reimbursed) & 
                eval(suppression_used) == "FALSE" & 
                (ndc %in% NDC | product_name %like% "HUMALOG"| product_name %like% "NOVOLOG")]
    return(out)
  })
  write.csv(rbindlist(data_list), file = "Cleaned Data/output_raw2023.csv", row.names = FALSE)
}
# Read-In and Plot =============================================================
key <- fread("Cleaned Data/key.csv")[, NDC := gsub("-", "", NDC)]
data <- fread("Cleaned Data/output_raw2023.csv", colClasses = c("ndc" = "character"))[key, on = c("ndc" = "NDC")
                                                                     ][ndc %in% NDC, 
                                                                       ][`Radiology Use` == "N"
                                                                         ][utilization_type == "FFSU"
                                                                           ][year > 2011]

# Prescription Reimbuirsed & Count =============================================
#Inflation ====
url <- "https://www.minneapolisfed.org/about-us/monetary-policy/inflation-calculator/consumer-price-index-1913-"
raw_tbl <- as.data.table(html_table(read_html(url))[[1]])
inflation <- setDT(raw_tbl)[Year > 2011, .(cpi = `Annual Average CPI(-U)`,
                                           year = Year)
                            ][, multiplier := as.numeric(raw_tbl[Year == 2023]$`Annual Average CPI(-U)`)/cpi, by = year
                              ][, year := as.numeric(year)
                                ][year != 2024]

Rx <- data[, .(rxCount = sum(number_of_prescriptions),
               amtReimbursed = sum(total_amount_reimbursed)), by = c("year", "Glucagon type")
           ][inflation, on = "year"
             ][, amtInflation := amtReimbursed*multiplier
               ]

costdata <- data[, .(total_reimbursement = sum(medicaid_amount_reimbursed),
                     nRx = sum(number_of_prescriptions)), by = c("year", "Glucagon type")
                 ][inflation, on = "year"
                   ][, amtInflation := total_reimbursement*multiplier
                     ][, ppRx := amtInflation/nRx
                       ][, cpi := NULL]
#write.csv(Rx, "Cleaned Data/reimbursements2023.csv")
#write.csv(costdata, "Cleaned Data/costData.csv", row.names = FALSE)
#Rx <- read.csv("reimbursements.csv")


p1 <- ggplot(unique(rbind(Rx, copy(Rx)[, `:=` (rxCount = sum(rxCount),
                        `Glucagon type` = "Total"), by = "year"])), aes(x = year, y = rxCount, col = `Glucagon type`)) + 
  geom_line() + 
  geom_point(alpha = .5) +
  scale_x_continuous(breaks = 2012:2023) + 
  theme_light() + 
  labs(color = "", y = "Prescription Count\n(Thousands)", x = "Year") + 
  theme(legend.position = "bottom",
        text = element_text(size = 16)) + 
  geom_vline(xintercept = 2019, col = "black", linetype = "dashed") +
  annotate("text", label = "Glucagon nasal spray\n introduced", x = 2020.5, y = 1.13e5, size = 5) + 
  guides(color=guide_legend(nrow=2, byrow=TRUE)) 
ggsave("Figures/RxCount.png", p1, height = 8, width = 10, dpi = 300)

p1.5 <- ggplot(unique(rbind(Rx, copy(Rx)[, `:=` (amtInflation = sum(amtInflation),
                                         `Glucagon type` = "Total"), by = "year"])), 
       aes(x = year, y = amtInflation, col = `Glucagon type`)) + 
  geom_line() + 
  geom_point(alpha = .5) +
  scale_x_continuous(breaks = 2012:2023) + 
  theme_light() + 
  labs(color = "", y = "Medicaid Reimbursement\n(Millions)", x = "Year") + 
  theme(legend.position = "bottom",
        text = element_text(size = 16)) + 
  geom_vline(xintercept = 2019, col = "black", linetype = "dashed") +
  scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6)) + 
  annotate("text", label = "Glucagon nasal spray\n introduced", x = 2020.5, y = 4e7, size = 5) + 
  guides(color=guide_legend(nrow=2, byrow=TRUE)) 

ggsave("Figures/amtReimbursed.png", p1.5, height = 8, width = 10, dpi = 300)

# Brand Percentage =============================================================
brand <- data[, rxTotal := sum(number_of_prescriptions), by = c("year", "Glucagon type")
              ][year > 2017
                ][, .("Year" = year, "Glucagon Type" = `Glucagon type`, rxTotal)] |>
  unique()
level_order <- tibble::deframe(brand[, sum(rxTotal), by = `Glucagon Type`]) |>
  sort() |>
  names()
brand <- brand[, `Glucagon Type` := factor(`Glucagon Type`, levels = level_order, ordered = TRUE,
                                           labels = c("Dasiglucagon, Auto-Injector",
                                                      "Glucagon, Pre-Filled Syringe",
                                                      "Glucagon, Auto-Injector",
                                                      "Glucagon, Nasal Spray",
                                                      "Glucagon, Unmixed Syringe"))
               ][, pct := rxTotal/sum(rxTotal)*100, by = "Year"]
setorder(brand, Year)
write.csv(brand, "Cleaned Data/brandpct.csv")
write.csv(brand[, .(total = sum(rxTotal)), by = "Year"
                ][, pctChange := round((total-shift(total))/shift(total)*100)], "Cleaned Data/total.csv")

p1 <- ggplot(brand, aes(x = Year, weight = rxTotal, group = `Glucagon Type`)) + 
  geom_bar(aes(fill = `Glucagon Type`), position = "fill") + 
  geom_text(aes(by = Year, 
                label = after_stat(ifelse(scales::percent(prop, accuracy = 0.01) < 1.98, NA, scales::percent(prop, accuracy = 1)))),
            stat = "prop",
            position = position_fill(vjust = .5), size = 4.7) + 
  scale_fill_manual(values = wesanderson::wes_palette("Darjeeling1", 5)) +
  theme_bw() + 
  theme(legend.position = "bottom", legend.justification = c(0.1, 0)) + 
  labs(fill = "", 
       y = "Percent") + 
  scale_y_continuous(labels = seq(0, 100, by = 25)) +
  scale_x_continuous(labels = 2018:2023, breaks = 2018:2023) +
  guides(fill = guide_legend(reverse = TRUE, nrow = 3)) +
  theme(text = element_text(size = 20))

#p2 <- p1 + ggtexttable(brand, rows = NULL, theme = ttheme(base_size = 12))
ggsave("Figures/pct.png", p1,  height = 10, width = 9, dpi = 300)



# Analogs (Reviewer Comment Response)
analog <- fread("Cleaned Data/output_raw2023.csv", colClasses = c("ndc" = "character"))[product_name %like% "HUMALOG" | product_name %like% "NOVOLOG", 
                                                                                        ][utilization_type == "FFSU"
                                                                                          ][year > 2011, 
                                                                                            ][, .(rxHumaNovolog = sum(number_of_prescriptions)), by = c("year")
                                                                                              ][Rx[, .(rxGlucagon = sum(rxCount)), by = "year"], on = "year"
                                                                                                ][, `Gluc/HumaNovolog` := rxGlucagon / rxHumaNovolog]
write.csv(analog, "Cleaned Data/analogs.csv", row.names = FALSE)
