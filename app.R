

library(htmltools)
library(shiny)
library(echarts4r)
library(bslib)
library(leaflet)
library(reactable)
library(fst)
library(tidyverse)
library(sf)
library(profvis)
# load(".RData")

# rm(past_populations,
#    instantiate_base_pop,
#    default_fracture4_female,
#    th,
#    test_poopulation,
#    teset_population,stroke_incidence,
#    population_w_established_prevalence,new_year_pop,wrapping_examples_in_function)
# rm(list=ls())

source('footer.R')
source('modules/startup_overlay/startup_overlay_div.R')

graph_wrapper <- function(..., header =NULL){
    
    div(class = "grid-item grid-item--graph theme-green",
        div(class = "grid-item-content",
            div(class = "chart-card",
                if(!is.null(header)){
                    div(class = "card-header",# style = "font-size: 0.5em;",
                        header
                    )},
                ...
            )
        )
    )
}

# theme_x <- readLines('theme.json')

ui <- page_fluid( id = 'main-content',
                  theme = bs_theme(version = 5, font_scale = 0.8,
                                   bootswatch = 'litera',
                                   primary = 'purple'), 

                  # startup_overlay_div(5000,7000),
                  
                  tags$head(
                  # Include external dependencies
            
                      # Packery CSS and JS from CDN (no Draggabilly needed)
                      tags$script(src = "https://unpkg.com/packery@2/dist/packery.pkgd.min.js"),
                      HTML('<script>
                        window.FontAwesomeConfig = {
                          searchPseudoElements: true
                        }
                      </script>'),
                      # tags$script(src = "roma.js"),
                      #includeScript("roma.js"),
                      HTML('<style>
.dashboard-nav{
--primary-navy: #1e3a8a;
  --primary-blue: #3b82f6;
  --secondary-blue: #60a5fa;
  --accent-orange: #f97316;
  --success-green: #10b981;
  --warning-yellow: #f59e0b;
  --danger-red: #ef4444;
  --neutral-gray: #6b7280;
  --light-gray: #f8fafc;
  --medium-gray: #e2e8f0;
  --dark-gray: #374151;
  --white: #ffffff;
  --glass-bg: rgba(255, 255, 255, 0.85);
--glass-border: rgba(255, 255, 255, 0.2);
--shadow-light: 0 1px 3px rgba(0, 0, 0, 0.1);
--shadow-medium: 0 4px 6px rgba(158, 123, 123, 0.07);
--shadow-heavy: 0 10px 25px rgba(0, 0, 0, 0.1);
--border-radius: 12px;
--border-radius-lg: 16px; 
--transition: all 0.2s cubic-bezier(0.4, 0, 0.2, 1);
--fa-style-family-brands: "Font Awesome 6 Brands";
--fa-font-brands: normal 400 1em/1 "Font Awesome 6 Brands";
--fa-font-regular: normal 400 1em/1 "Font Awesome 6 Free";
--fa-style-family-classic: "Font Awesome 6 Free";
--fa-font-solid: normal 900 1em/1 "Font Awesome 6 Free";
color: var(--dark-gray);
line-height: 1.6;
margin: 0;
box-sizing: border-box;
animation-duration: 0.01ms !important;
animation-iteration-count: 1 !important;
transition-duration: 0.01ms !important;
background: var(--white);
border-bottom: 1px solid var(--medium-gray);
padding: 1rem 2rem;
margin: 4rem 0rem;

box-shadow: var(--shadow-light);
}
</style>'),
                      # Custom CSS styling
                      tags$style("
* { box-sizing: border-box; } 

body { 
  /*font-family: sans-serif;  */
  margin: 0;
  padding: 0;
}

/* ---- Layout ---- */

.main-container {
  display: flex;
  min-height: 100vh;
}

.sidebar {
  width: 250px;
  background: linear-gradient(180deg, #2c3e50 0%, #34495e 100%);
  color: white;
  position: fixed;
  height: 100vh;
  left: 0;
  top: 0;
  overflow-y: auto;
  box-shadow: 2px 0 10px rgba(0,0,0,0.1);
  z-index: 1000;
}

.sidebar-header {
  padding: 20px;
  border-bottom: 1px solid rgba(255,255,255,0.1);
  text-align: center;
}

.sidebar-title {
  font-size: 1.4em;
  font-weight: bold;
  margin: 0;
}

.nav-section {
  padding: 15px 0;
}

.nav-section-title {
  padding: 10px 5px;
  font-size: 1em;
  color: darkgrey;
  /*text-transform: uppercase;*/
  letter-spacing: 1px;
  margin: 0;
}

.nav-item {
  color: grey;
  padding: 3px 3px;
  margin: 2px 0px;
  cursor: pointer;
  transition: all 0.3s ease;
  border-left: 3px solid transparent;
}

.nav-item:hover {
  background: rgba(52, 152, 219, 0.1);
  border-radius:10px;

}

.nav-item.active {
  /*background: rgba(52, 152, 219, 0.5);
  color:white; */
  font-weight: bold;
  border-radius:10px;

}

.nav-iten.active{
  background-color: white;
  color: black !important;
  font-weight: normal;
  opacity:1;
  background-opacity:1;

}

a.nav-iten:hover{

 background-color: white;
 opacity:1;
 background-opacity:1;

}


.nav-icon {
  margin-right: 10px;
  width: 16px;
  display: inline-block;
}

.content-area {
  margin:auto;
  flex: 1;
  /*padding: 20px;*/
}
2
/* ---- grid ---- */
.grid {
  padding: 10px;
  border-radius: 40px;
  max-width: 70vw;
  min-height: 100vh;
}
2
/* clear fix */
.grid:after {
  content: '';
  display: block;
  clear: both;
}

/* ---- .grid-item ---- */
.grid-item {
  float: left;
  background: white;
  border-radius:20px;
  box-shadow: 0 1px 3px rgba(100, 0, 0, 0.01);
  border: 1px solid hsla(0, 0%, 100%, 0.5);
  transition: all 0.3s ease;
  cursor: pointer;
  overflow: hidden;
  margin-right: 2px;
  margin-bottom: 2px;
}

.grid-item-content {22
  padding: 20px;
  border-radius: 20px;
  width: 100%;
  height: 100%;
  transition: all 0.4s ease;
}

.grid-item--width2 { width: 150px; }
.grid-item--height2 { height: 150px; }
.grid-item--small { width: 150px; height: 150px; }
.grid-item--graph { width: 500px; height: 400px; }

/* Expanded state */
.grid-item.is-expanded {
  z-index: 100;
}

.grid-item.is-expanded.grid-item--small {
  width: 400px;
  height: 300px;
}

.grid-item.is-expanded.grid-item--graph {
  width: 700px;
  height: 600px;
}

.grid-item.is-expanded .grid-item-content {
  transform: scale(1.02);
}

.grid-item:hover {
 /* 
  margin-right: 0px;
  margin-bottom: 0px; 
  */
  transform: translate(2px 2px);
  z-index:1000;
  box-shadow: 0 4px 12px rgba(0, 0, 0, 0.15);
}

.grid-item:hover .grid-item-content {
  background: rgba(255, 255, 255, 0.95);
}

/* Click indicator */
.grid-item::after {
  position: absolute;
  top: 10px;
  right: 10px;
  opacity: 0;
  transition: opacity 0.3s ease;
  font-size: 12px;
  background: rgba(0, 0, 0, 0.3);
  color: white;
  padding: 4px 6px;
  border-radius: 4px;
}

.grid-item:hover::after {
  opacity: 1;
}

.grid-item.is-expanded::after {
  content: '\f16c';
    /* >> Name of the FA free font (mandatory), e.g.:
               - 'Font Awesome 5 Free' for Regular and Solid symbols;
               - 'Font Awesome 5 Pro' for Regular and Solid symbols (Professional License);
               - 'Font Awesome 5 Brand' for Brands symbols. */
    font-family: 'Font Awesome 5 Free';
    /* >> Weight of the font (mandatory):
               - 400 for Regular and Brands symbols;
               - 900 for Solid symbols;
               - 300 for Light symbols. */
    font-weight: 400;
    opacity:1;
}

/* Metric Cards */
.metric-card {
  margin:auto;
  text-align: center;
  height: 100%;
  width: 100px;
  display: flex;
  flex-direction: column;
  justify-content: center;
}

.handle{
  position:relative;
  top:1px;
  left:15px;
}

.metric-value {
  font-size: 2.1em;
  font-weight: bold;
  margin-bottom: 10px;
}

.metric-label {
  font-size: 1em;
  color: #6c757d;
  margin-bottom: 15px;
}

.metric-change {
  font-size: 0.8em;
  font-weight: 600;
  padding: 5px 15px;
  border-radius: 20px;
  display: inline-block;
}

.metric-change.positive {
  background: rgba(40, 167, 69, 0.1);
  color: #28a745;
}

.metric-change.negative {
  background: rgba(220, 53, 69, 0.1);
  color: #dc3545;
}

/* Chart Cards */
.chart-card {
  padding: 15px;
  height: 100%;
  width: 100%;
  display: flex;
  flex-direction: column;
}

.chart-card .echarts4r {
  flex: 1;
  min-height: 0;
  width: 100% !important;
  height: 100% !important;
}

.card-header {
  font-weight: bold;
  margin-bottom: 15px;
  color: #495057;
  border-bottom: 2px solid #e9ecef;
  padding-bottom: 10px;
}

/* Control Panel */
.control-panel {
  background: white;
  padding: 20px;
  border-radius: 12px;
  margin-bottom: 20px;
  box-shadow: 0 2px 10px rgba(0,0,0,0.1);
}

/* Navigation Cards*/
.nav-card {
  color: black;
  opacity:0.7;
  display: flex;
  flex-direction: column;
  justify-content: center;
  cursor: pointer;
  margin:0.5rem;
  padding:0.5rem;
  border-radius:0.5rem;
  transition:all 0.5s ease;
} 

.nav-card:hover {
    opacity:0.9;
  transform: scale(1.02);
  z-index:10;
  transition:all 0.3s ease;
}

.nav-card-icon {
  color:white;
  text-align: right;
  font-size: 1.4em;
  margin-bottom: 0.5rem;
  opacity: 0.9;
}

/* Tab Content Styling */
.tab-content {
  display: block;
  padding: 20px;
  animation: fadeIn 0.3s ease-in-out;
}

.tab-content.active {
  display: block;
}

@keyframes fadeIn {
  from { opacity: 0; }
  to { opacity: 1; }
}

.nav-card-icon i:hover {
  box-shadow: 0 1px 1px rgba(255,255,255,0.1);
}

.nav-card-title {
  font-size: 1.3em;
  font-weight: bold;
  margin-bottom: 8px;
}

a{
text-decoration:none !important;
}

.nav-card-description {
  font-size: 0.8em;
  opacity: 0.8;
}

.nav-card.reports {
  background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%);
}

.nav-card.settings {
  background: linear-gradient(135deg, #4facfe 0%, #00f2fe 100%);
}

.nav-card.analytics {
  background: linear-gradient(135deg, #43e97b 0%, #38f9d7 100%);
}

  .glass-card {
  background: rgba(255, 255, 255, 0.44); /* 0.14 */
  backdrop-filter: blur(5px);
  -webkit-backdrop-filter: blur(5px);
  border-radius: 20px;
  border: 1px solid rgba(255, 255, 255, 0.3);
  box-shadow: 
    0 8px 32px rgba(0, 0, 0, 0.1),
    inset 0 1px 0 rgba(255, 255, 255, 0.5),
    inset 0 -1px 0 rgba(255, 255, 255, 0.1),
    inset 0 0 10px 5px rgba(255, 255, 255, 0.5);
  overflow: hidden;
}

.glass-card::before {
  content: '';
  position: absolute;
  top: 0;
  left: 0;
  right: 0;
  height: 1px;
  background: linear-gradient(
    90deg,
    transparent,
    rgba(255, 255, 255, 0.8),
    transparent
  );
}

.glass-card::after {
  content: '';
  position: absolute;
  top: 0;
  left: 0;
  width: 1px;
  height: 100%;
  background: linear-gradient(
    180deg,
    rgba(255, 255, 255, 0.8),
    transparent,
    rgba(255, 255, 255, 0.3)
  );
}

")
                  ),
                  
                  # Main layout container

                  # Control Panel

                 
                  # HTML(' <!-- Navigation -->
                  #    <nav class="dashboard-nav">
                  #    <div class="nav-content">
                  #    <div class="nav-items">
                  #    <a href="#overview" class="nav-item active" data-section="overview" data-bs-toggle="tab" data-bs-target="#overview">
                  #    <i class="fas fa-tachometer-alt"></i>
                  #    <span>Overview</span>
                  #    </a>
                  #    <a href="#geography" class="nav-item" data-section="geography"  data-bs-toggle="tab" data-bs-target="#geography">
                  #    <i class="fas fa-map-marked-alt"></i>
                  #    <span>Geographic Analysis</span>
                  #    </a>
                  #    <a href="#population" class="nav-item" data-section="population"  data-bs-toggle="tab" data-bs-target="#population">
                  #    <i class="fas fa-users"></i>
                  #    <span>Population Insights</span>
                  #    </a>
                  #    <a href="#risk-factors" class="nav-item" data-section="risk-factors"  data-bs-toggle="tab" data-bs-target="#risk-factors">
                  #    <i class="fas fa-exclamation-triangle"></i>
                  #    <span>Risk Factors</span>
                  #    </a>
                  #    <a href="#data-explorer" class="nav-item" data-section="data-explorer"  data-bs-toggle="tab" data-bs-target="#data-explorer">
                  #    <i class="fas fa-table"></i>
                  #    <span>Data Explorer</span>
                  #    </a>
                  #    </div>
                  #    </div>
                  #    </nav>'),

                  shiny::tags$nav(
                      style='position:fixed;top:14px;left:0px',
                      class = "navbar w-100 shadow-lg glass-card mb-3 p-2 mt-5 rounded", # bg-light
                      shiny::h4(  "Population Health", span(class = 'lead','Public Health Agency'),
                                  
                                  HTML('<h7><span class="badge rounded-pill bg-opacity-75 bg-warning ">ADHD </span></h7>'),
                                  span(HTML('<h7><span class="badge rounded-pill text-white bg-opacity-75 bg-primary">PHA', '' ,'</span></h7>')),
                                  HTML('<h7><span class="badge rounded-pill bg-opacity-75 bg-info ">Population Health Model </span></h7>'),
                                  HTML('<h7 id = "move" style = "float:right;">
                       <!-- <a href="https://apply-for-innovation-funding.service.gov.uk/competition/2186/overview/e51c18bc-21b3-450d-bdbc-2f43dad3b268">
                       <span class="badge rounded-pill bg-light">
                        <i class="fa-solid fa-arrow-up-right-from-square"></i>
                       OPIP bid</span>
                       </a> -->
                       </h7>'),
                                  
                                  class = "navbar-brand mb-0 w-100"
                      )
                  ),
                  
                  div(style='top:20vh;width:12vw;z-index:1000',class = ' ms-2  p-3 d-flex flex-column display-absolute position-fixed left-0 ',#shadow-sm glass-card
                      
                      div(class = "nav-section",
                          h6(class = "bg-opacity-100 border-5  p-2 rounded-3  text-bg-dark text-light", "Health Burden"), #text-body-secondary
                          div(class = "nav-item",
                              span(class = "nav-icon"),
                              "Dashboard"
                          )#,
                          # div(class = "nav-item active",
                          #     span(class = "nav-icon"),
                          #     "Analytics"
                          # ),
                          # div(class = "nav-item",
                          #     span(class = "nav-icon"),
                          #     "Reports"
                          #)
                      ),
                      
                      # div(class = "nav-section",
                      #     h6(class = "bg-opacity-100 border-5  p-2 rounded-3  text-bg-dark text-light",
                      #        "ADHD"),
                      #     
                      #     div(class = "nav-item",
                      #         span(class = "nav-icon"),
                      #         "Risk"
                      #     )#,
                          
                          # div(class = "nav-item",
                          #     span(class = "nav-icon"),
                          #     "Geography"
                          # ),
                          # div(class = "nav-item",
                          #     span(class = "nav-icon"),
                          #     "Population"
                          # ),
                          # 
                          # div(class = "nav-item",
                          #     span(class = "nav-icon"),
                          #     "Northern Trust"
                          # ),
                          # div(class = "nav-item",
                          #     span(class = "nav-icon"),
                          #     "Lifestyle"
                          # ),
                          # div(class = "nav-item",
                          #     span(class = "nav-icon"),
                          #     "Deprivation"
                          # ),
                          # div(class = "nav-item",
                          #     span(class = "nav-icon"),
                          #     "Society"
                          #)
                      # )#,
                      # div(class = "nav-section",
                      #     h6(class = "bg-opacity-100  border-5  p-2 rounded-3 text-bg-dark text-light", "Intervention"), #nav-section-title
                      #     div(class = "nav-item",
                      #         span(class = "nav-icon"),
                      #         "Specify"
                      #     ),
                      #     div(class = "nav-item",
                      #         span(class = "nav-icon"),
                      #         "Scenarios"
                      #     )
                      # )
                      
                  ),
                  
                  div(class='mt-5 pt-5'),
                  HTML('<nav id = "top-top" 
                                  style = "position:fixed;top:0px;margin-inline:-12px;backdrop-filter: blur(5px);background: #00D4FF;
                                  background: linear-gradient(55deg, rgba(0, 212, 255, 06) 55%, rgba(0, 115, 255, 0.7) 87%);", 
                                  class="w-100 bg-gradient-blue-cyan bg-opacity-50">
                      <div class="text-white">
                      <div id ="top-nav-content" class="d-flex gap-5 justify-content-center align-items-center">
                      <a href="#overview" class="nav-item bg-opacity-100  nav-iten p-1 px-4 rounded-pill text-white" data-section="overview" data-bs-toggle="tab" data-bs-target="#overview">
                      <i class="fas fa-tachometer-alt"></i>
                      <span>Overview</span>
                      </a>
                      <a href="#geography" class="nav-item bg-opacity-100  nav-iten p-1 px-4 rounded-pill text-white" data-section="geography" data-bs-toggle="tab" data-bs-target="#geography">
                      <i class="fas fa-map-marked-alt"></i>
                      <span>Geographic Analysis</span>
                      </a>
                      <a href="#population" class="nav-item bg-opacity-100  nav-iten p-1 px-4 rounded-pill text-white active" data-section="population" data-bs-toggle="tab" data-bs-target="#population">
                      <i class="fas fa-users"></i>
                      <span>Population Insights</span>
                      </a>
                      <a href="#risk-factors" class="nav-item bg-opacity-100  nav-iten p-1 px-4 rounded-pill text-white" data-section="risk-factors" data-bs-toggle="tab" data-bs-target="#risk-factors">
                      <i class="fas fa-exclamation-triangle"></i>
                      <span>Risk Factors</span>
                      </a>
                      <a href="#data-explorer" class="nav-item bg-opacity-100  nav-iten m-3 p-1 px-4 rounded-pill text-white" data-section="data-explorer" data-bs-toggle="tab" data-bs-target="#data-explorer">
                      <i class="fas fa-table"></i>
                      <span>Data Explorer</span>
                      </a>
                      </div>
                      </div>
                      </nav>'),
                  
                  div(class = "main-container",
                      
                      # Main content area
                      div(class = "content-area",
                          # div( class="tab-pane active", id="overview", role="tabpanel", `aria-labelledby`="overview",
                          
                          div(class = "tab-content",
                              
                              # Dashboard Tab Content
                              
                              div(id = "dashboard-tab", class = "tab-pane show active", #
                                
                                  div(class = "container-fluid",
                                      style = "padding-left: 17%;padding-top:10%",

                                      div(class="alert alert-light my-5 opacity-100 shadow-sm", 
                                          h5('ADHD Estimate in the population'),
                                          span(class='lead','Comparison of approaches for prevalence and incidence')),
                                      
                                      div(class='d-flex flex-row py-5  gap-5 flex-wrap',#grid
                                                          
graph_wrapper(plotOutput('prevalence_rates_graph') ,header=span(span(class='fs-5','ADHD Prevalence') ,
                                                                span(class='m-1  badge rounded-pill text-bg-secondary','Rates'), div(class='text-muted','Estimated Actual')),
              div(class='p-3 rounded-2 text-bg-light','Comparison of simulations from the paper',icon("arrow-up-right-from-square"), 'with extrapolated and prevalence and incidence rates',
                  'also showing is thengland experimantl statistics ',icon("arrow-up-right-from-square") ,
                  'with nice prevlaence applied',icon("arrow-up-right-from-square"),
                  'and paramters lifted from the teams estimates')),

graph_wrapper(plotOutput('prevalent_counts_graph') ,header=span('Prevalence' ,span(class='ms-2  badge rounded-pill text-bg-secondary','Counts'),
                                                                span(class='ms-2 p-1 text-bg-light','Estimated Actual')),
              div(class='p-3 rounded-2 text-bg-light','Counts of the same')),


graph_wrapper(plotOutput('prevalence_my_sim_graph') ,header=span('Prevalence' ,span(class='ms-2  badge rounded-pill text-bg-secondary','Counts'),
                                                                 span(class='ms-2  badge rounded-pill text-bg-dark','Simulation'),
                                                                 span(class='ms-2 p-1 text-bg-dark','Estimated GP recorded')),
              div(class='p-3 rounded-2 text-bg-light','Counts of the same')),

),

div(class="alert alert-light fs-5 my-5 opacity-100 shadow-sm", h5('Incidence' ), span(class='lead','Transitions of Children to adult services. Incidence is largely interpreted as diagnosis.')),
div(class='d-flex flex-row gap-5 py-5 min-vh-100 flex-wrap',#grid

graph_wrapper(plotOutput('yearly_incidence_graph') ,header=span('Incidence' ,span(class='ms-2  badge rounded-pill text-bg-secondary','Counts'),span(class='ms-2 p-1 text-bg-dark','Yearly')),
              div(class='p-3 rounded-2 text-bg-light','Counts of the same')),

graph_wrapper(plotOutput('cumulative_incidence_graph') ,header=span('Incidence' ,span(class='ms-2 badge rounded-pill text-bg-secondary','Counts'),span(class='ms-2 p-1 text-bg-dark','Cumulative')),
              div(class='p-3 rounded-2 text-bg-light','Counts of the same')),

graph_wrapper(plotOutput('new_adult_transitions') ,header=span('18 year olds Transitioning to Adults' ,span(class='ms-2  badge rounded-pill text-bg-secondary','Counts'),span(class='ms-2 p-1 text-bg-dark','Cumulative')),
              div(class='p-3 rounded-2 text-bg-light','Counts of the same'))

),


div(class="alert alert-light fs-5 my-5 opacity-100 shadow-sm", h5('Referrals'), span(class='lead','Adults, Children and Trusts')),
div(class='d-flex flex-row gap-5 py-5 flex-wrap',#grid

graph_wrapper(plotOutput('adult_referals') ,header=span('Adult Referrals' ,span(class='ms-2 text-muted badge rounded-pill','Counts'),span(class='ms-2 p-1 text-bg-dark','Estimated Actual')),
              div(class='p-3 rounded-2 text-bg-light','Counts of the same')),
graph_wrapper(plotOutput('accepted_adult_referals') ,header=span('Accepted Adult Referrals' ,span(class='ms-2  badge rounded-pill text-bg-secondary','Counts'),span(class='ms-2 p-1 text-bg-dark','Cumulative')),
              div(class='p-3 rounded-2 text-bg-light','Counts of the same')),

graph_wrapper(plotOutput('child_referals') ,header=span('Child Referals' ,span(class='ms-2  badge rounded-pill text-bg-secondary','Counts'),span(class='ms-2 p-1 text-bg-dark','Cumulative')),
              div(class='p-3 rounded-2 text-bg-light','Counts of the same')),
graph_wrapper(plotOutput('accepted_child_referals') ,header=span('Accepted Child Referrals' ,span(class='ms-2  badge rounded-pill text-bg-secondary','Counts'),span(class='ms-2 p-1 text-bg-dark','Cumulative')),
              div(class='p-3 rounded-2 text-bg-light','Counts of the same'))

),






                                                 
                                                 div(class = "grid-item nav-card analytics bg-opacity-50",
                                                     div(onclick = "$('.tab-pane').removeClass('active show');$('#' + 'analytics-tab').addClass('active show')",
                                                         class = "nav-card-icon",
                                                         icon("arrow-up-right-from-square")),
                                                     div(class = "nav-card-title", "Morbidity"),
                                                     div(class = "nav-card-description", "View detailed analytics"),
                                                 ),
                                                 div(class = "grid-item nav-card settings bg-opacity-50",
                                                     div(onclick = "$('.tab-pane').removeClass('active show');$('#' + 'geography-tab').addClass('active show')",
                                                         class = "nav-card-icon",
                                                         icon("arrow-up-right-from-square")),
                                                     div(class = "nav-card-title", "Geography"),
                                                     div(class = "nav-card-description", "View detailed analytics"),
                                                 ),
                                                 div(class = "grid-item nav-card  bg-opacity-50",
                                                     div(onclick = "$('.tab-pane').removeClass('active show');$('#' + 'deprivation-tab').addClass('active show')",
                                                         class = "nav-card-icon",
                                                         icon("arrow-up-right-from-square")),
                                                     div(class = "nav-card-title", "Deprivation"),
                                                     div(class = "nav-card-description", "View detailed analytics"),
                                                 ),
                                                 
                                                 
                                          ) # End of dashboard grid
                                      #, # End of dashboard tab content
                                      
                                      
                                  
                              ),
                              
                              # Analytics Tab Content
                              div(id = "analytics-tab", class = "tab-pane active show ", #style = "width:100%",
                                  div(style = 'position:absolute;top:0;left:0;margin-inline:-12px;',#class = "container-fluid",
                                      div(
                                          #leafletOutput("mymap", width = '100vw', height = '100vh'),
                                      ),
                                  ),
                                  
                                  div(class = 'glass-card p-3 me-3',
                                      style = 'position:absolute;top:12vh;right:0;height:75vh; overflow:scroll; background: rgba(255, 255, 255, 0.64)!important; /* 0.14 */',#class = "container-fluid",
                                      # style='top:15vh;width:15vw;z-index:1000',class = ' ms-3 p-4 d-flex flex-column display-absolute position-fixed left-0 shadow-sm glass-card'
                                      div(class = 'd-flex flex-column gap-3 justify-content-between',
                                          div( span(class = '','BMI'),
                                               div(style = 'height:150px;width:200px;')
                                          ),
                                          div( span(class = '','Age'),
                                               div(style = 'height:150px;width:200px;')
                                          ),
                                          div(span(class = '','Sex'),
                                              div(style = 'height:150px;width:200px;')
                                          ),
                                          div(span(class = '','Deprivation'),
                                              div(style = 'height:150px;width:200px;')
                                          ),
                                          div(span(class = '','Risk'),
                                              div(style = 'height:250px;width:200px;')
                                          ),
                                          div(span(class = '','Risk'),
                                              div(style = 'height:200px;width:200px;')
                                          ),
                                          
                                          span( class='ms-4',
                                                h6('Population:',textOutput('headline_count'))
                                          ),
                                          span( class='ms-4',
                                                h6('Qrisk Score:',textOutput('qrisk_average'))
                                          )
                                      )
                                  )
                                  
                              ),
                              
                              # Reports Tab Content
                              div(id = "reports-tab", class = " tab-pane active show",# style = "display: none;",
                                  div(class = "container-fluid", style = "padding-left: 20%;",
                                      h3("Population Health Data - Interactive Pivot Analysis"),
                                      p(class='lead',"Drag columns to create custom analysis. Use dimensions for grouping, measures for aggregation."),
                                      div(style = "font-size: 0.6rem !important;",
                                          # Pivot module UI
                                          pivot_module_ui("pivot_reports",
                                                          data_names = c("sex", "age_risk", "county", "hsct", "bmi",
                                                                         "Urban_status", "mdm_quintile_soa_name", "ethnicity",
                                                                         "stroke", "chd", "diabetes", "dementia", "heart_failure"),
                                                          fun_names = c("sum","mean","median","min","max","count","n_distinct")),

                                      )
                                  )
                              ),
                              
                              # Orders Tab Content
                              div(id = "ObesityRisk-tab", class = "  tab-pane show active ", style = "",
                                  div( style = "padding-left: 17%;",
                                       h3("Obesity Contributions to Obesity"),
                                       p(class = 'lead', "Factors leading to the cause of Obesity and often accompany it ")
                              ),
                              
                              div(id = "geography-tab", class = "tab-pane show active", style = " ",
                                  div(class = "container-fluid",   style = "padding-left: 14%", #;width:100%
                                      h3("Geography Analytics"),
                                      p(class = 'lead',"A supplement to the <a onclick= alert('hello')>analytics</a> showing the heirarchical ")
                              )
                              ),
                              
                              div(id = "population-tab", class = "tab-pane show active ", style = " ",
                                  div(class = "container-fluid",   style = "padding-left: 17%;",
                                      h3(" Obesity Analytics"),
                                      
                                      p(class = 'lead', "Analysis of Obesity dynamic in the Population"),

                                  )
                              ),
                              
                              
                              div(id = "lifestyle-tab", class = "  tab-pane show active ", style = " ",
                                  div(style = "padding-left: 17%;",
                                      h3("Lifestyle Dashboard"),
                                      p(class = 'lead',"Deprivation content will be displayed here when the Users nav item is clicked."),
                                      h6('Health Burden Attributable to obesity'),
                                    
       
                                  )
                              ),
                              
                              # Users Tab Content
                              div(id = "deprivation-tab", class = "  tab-pane active show", style = " ",
                                  div(class = "container-fluid",   style = "padding-left: 15%;",
        
                              ),
                              
                              
                              
                              div(id = "society-tab", class = "tab-pane show active", style = " ",
                                  div(class = "container-fluid",   style = "padding-left: 17%;",
                                   
                                  )
                              ),
                              
     
                              
            
                          ) # End of tab-content-container
                          
                      ), # Close content-area
                      
                      # Initialize Packery with Click and Expand JavaScript
                      tags$script(HTML("
$(document).ready(function() {
  // Initialize Packery when the page loads
  setTimeout(function() {
    var $grid = $('.grid').packery({
      itemSelector: '.grid-item',
      columnWidth: 80,
      gutter:10
    });

    // Handle click events on grid items
    $grid.on('doubleclick', '.grid-item-content', function(event) {
      event.preventDefault();
      event.stopPropagation();
      
      var itemElem = event.currentTarget.parentNode;
      var $item = $(itemElem);
      var isExpanded = $item.hasClass('is-expanded');
      
      // Toggle the expanded class
      $item.toggleClass('is-expanded');
      
      // Force a layout update after the CSS transition
      setTimeout(function() {
        if (isExpanded) {
          // If contracting, use shiftLayout to compact everything
          $grid.packery();
        } else {
          // If expanding, first layout normally, then fit the expanded item
          $grid.packery('shiftLayout');
          setTimeout(function() {
            $grid.packery('shiftLayout', itemElem);
          }, 50);
        }
      }, 50); // Small delay to let CSS transition start
      
      // Also trigger layout after CSS transition completes
      setTimeout(function() {
        $grid.packery();
      }, 150); // Match CSS transition duration (0.4s + buffer)
    });
    
    $('.chart-card').on('click', function() {
      console.log('resize');
      // Trigger ECharts resize
      setTimeout(function() {
        $('.echarts4r').each(function() {

          //if (this.echartsInstance) {
          console.log('resizeIn');
          console.log(this);
          echarts.getInstanceByDom(this).resize();
            //this.echartsInstance.resize();
          //}
        });
      }, 300);
    });
    
    // Tab Navigation Functionality
    $('.nav-item').on('click', function() {
      // Remove active class from all nav items
      $('.nav-item').removeClass('active');
      // Add active class to clicked item
      $(this).addClass('active');
      
      // Hide all tab content
      $('.tab-pane').removeClass('active show')
      
      // Show corresponding tab content based on nav item text
      var navText = $(this).text().trim().toLowerCase();
      var tabId = '';
      
      switch(navText) {
        case 'dashboard':
          tabId = 'dashboard-tab';
          break;
        case 'analytics':
          tabId = 'analytics-tab';
          break;
        case 'reports':
          tabId = 'reports-tab';
          break;
        case 'geography':
          tabId = 'geography-tab';
          break;
        case 'deprivation':
          tabId = 'deprivation-tab';
          break;
        case 'population':
          tabId = 'population-tab';
          break;
        case 'intervention':
          tabId = 'intervention-tab';
          break;
        case 'specify':
          tabId = 'specify-tab';
          break;
        case 'scenarios':
          tabId = 'scenarios-tab';
          break;
        case 'obesity risk':
          tabId = 'ObesityRisk-tab';
          break;
        case 'northern trust':
          tabId = 'NorthernTrust-tab';
          break;
        case 'lifestyle':
          tabId = 'lifestyle-tab';
          break;
         case 'society':
          tabId = 'society-tab';
          break;
        default:
          tabId = 'dashboard-tab';
      }
      
      $('#' + tabId).addClass('active show')
      
      // Re-initialize packery if dashboard tab is shown
      if (tabId === 'dashboard-tab') {
        setTimeout(function() {
          $('.grid').packery();
        }, 100);
      }
      
      // Initialize pivot module if reports tab is shown
      if (tabId === 'reports-tab') {
        setTimeout(function() {
          // Trigger pivot module column update
          if (window.Shiny && window.Shiny.setInputValue) {
            window.Shiny.setInputValue('pivot_reports_tab_shown', Math.random());
          }
          
          // Re-trigger any drag-drop initialization if needed
          if (typeof window.setupDropzone === 'function') {
            console.log('Re-initializing pivot drag-drop zones');
            window.setupDropzone('#column-pool');
            window.setupDropzoneCat('#groups-drop');
            window.setupDropzoneCat('#wide-by-drop');
            window.setupDropzone('#values-drop');
            window.setupDropzoneValueFunc('#value-func-drop');
          }
        }, 200);
      }
    });
    
    
    // setTimeout(function() {
    //  $('.tab-pane').removeClass('active show')
    //  $('#' + 'specify-tab').addClass('active show')
    // }, 1000);
     
      setTimeout(function() {
          $('.grid').packery();
        }, 100);
     
  }, 500); // Small delay to ensure DOM is ready
   
});
"
                      ))), # tab pane
                  
)),

    footer()
)

# ============================================================================
# SERVER
# ============================================================================
server <- function(input, output, session) {
    
    output$prevalence_rates_graph <- renderPlot(prevalence_rates_graph)
    output$prevalent_counts_graph <- renderPlot(prevalent_counts_graph)
    output$prevalence_my_sim_graph <- renderPlot(prevalence_my_sim_graph)
    output$yearly_incidence_graph <- renderPlot(yearly_incidence_graph)
    output$cumulative_incidence_graph <- renderPlot(cumulative_incidence_graph)
    output$new_adult_transitions <- renderPlot(new_adult_transitions)
    output$new_adult_transitions <- renderPlot(new_adult_transitions)
    output$adult_referals <- renderPlot(adult_referals)
    output$accepted_adult_referals <- renderPlot(accepted_adult_referals)
    output$child_referals <- renderPlot(child_referals)
    output$accepted_child_referals <- renderPlot(accepted_child_referals)

    
}


shinyApp(ui = ui, server = server)
