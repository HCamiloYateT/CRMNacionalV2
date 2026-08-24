header <- bs4DashNavbar(status = "white", border = FALSE, sidebarIcon = icon("bars"),
                        title = dashboardBrand(title = tit_app,
                                               href = "https://analitica.racafe.com/PortalAnalitica/",
                                               image = "https://raw.githubusercontent.com/HCamiloYateT/Compartido/refs/heads/main/img/logo2.png"),
                        controlbarIcon = icon("gears"),
                        leftUi = tagList(
                          tags$li(class = "dropdown",
                                  style = "display:flex;align-items:center; gap:8px;padding:8px 12px;cursor:default;",
                                  tags$span(uiOutput("user")),
                                  tags$span(style = "color:#ddd;font-size:0.85rem;", "|"),
                                  tags$span(uiOutput("group")),
                                  racafeShiny::Boton("BTN_Actualizar", label = NULL, icono = "sync",
                                                     size = "xxs", label_posicion = "below",
                                                     titulo = "Actualizar")
                                  )
                          ),
                        rightUi = tagList(
                          tags$li(class = "dropdown",
                                  tags$a(style = paste("display:flex;align-items:center;", "gap:14px;padding:8px 12px;cursor:default;"),
                                         tags$span(style = "font-size:0.79rem;color:#999;",
                                                   icon("circle", style = "color:green;font-size:0.64rem;"),
                                                   " Datos simulados"),
                                         tags$span(style = "font-size:0.79rem;color:#999;",
                                                   icon("calendar-alt"), paste0(" ", format_fecha_es(Sys.Date()))
                                                   )
                                         )
                                  )
                          )
                        )
