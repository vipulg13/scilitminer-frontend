library(shiny)

timeoutSeconds <- 300
inactivity <- sprintf("function idleTimer() {
  var t = setTimeout(logout, %s);
  window.onmousemove = resetTimer; // catches mouse movements
  window.onmousedown = resetTimer; // catches mouse movements
  window.onclick = resetTimer;     // catches mouse clicks
  window.onscroll = resetTimer;    // catches scrolling
  window.onkeypress = resetTimer;  //catches keyboard actions
  function logout() {
    Shiny.setInputValue('timeOut', '%ss', {priority: 'event'})
  }
  function resetTimer() {
    clearTimeout(t);
    t = setTimeout(logout, %s);  // time is in milliseconds (1000 is 1 second)
  }
}
idleTimer();", timeoutSeconds*1000, timeoutSeconds, timeoutSeconds*1000)

ui <- fluidPage(
  shinyjs::useShinyjs(),
  tags$script(inactivity),
  tags$head(
    tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "scilitminer-logo.png"),
    tags$style(HTML("
      @font-face {
        font-family: 'Audiowide';
        src: url(Audiowide-Regular.ttf) format('truetype');
      }
      @font-face {
        font-family: 'Montserrat';
        src: url(Montserrat-Medium.ttf) format('truetype');
      }
      body {
        font-family: 'Montserrat';
      }
      pre {
        font-family: 'Montserrat';
      }
      .navbar-nav { float: none !important; }
      .navbar-nav > li:nth-child(1) { float: right; }")),
    tags$script(HTML("
      $(document).on('keypress', function(e) {
        // Check if the active tab is 'login_element'
        if ($('.tab-pane.active').attr('data-value') === 'login_element') {
          // Check if the Enter key (key code 13) was pressed
          if (e.which === 13) {
            // Trigger the click event on the login button
            $('#cust_id-login_button').click();
          }
        }
      });
    ")),
    tags$style(HTML("
      /* Loading screen style */
      #loading-screen {
        position: fixed;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        background-color: white;
        z-index: 9999;
        display: flex;
        align-items: center;
        justify-content: center;
        font-size: 24px;
        color: black;
      }
    "))
  ),
  
  # Loading screen
  div(id = "loading-screen",
      div(class = "dot-floating"),
      tags$style(HTML("
        .dot-floating {
          position: relative;
          width: 10px;
          height: 10px;
          border-radius: 5px;
          background-color: hsl(0deg, 100%, 0%);
          animation: dot-floating 3s infinite cubic-bezier(0.15, 0.6, 0.9, 0.1);
        }
        .dot-floating::before, .dot-floating::after {
          content: '';
          display: inline-block;
          position: absolute;
          top: 0;
        }
        .dot-floating::before {
          left: -12px;
          width: 10px;
          height: 10px;
          border-radius: 5px;
          background-color: hsl(0deg, 100%, 0%);
          animation: dot-floating-before 3s infinite ease-in-out;
        }
        .dot-floating::after {
          left: -24px;
          width: 10px;
          height: 10px;
          border-radius: 5px;
          background-color: hsl(0deg, 100%, 0%);
          animation: dot-floating-after 3s infinite cubic-bezier(0.4, 0, 1, 1);
        }
        
        @keyframes dot-floating {
          0% {
            left: calc(-50% - 5px);
          }
          75% {
            left: calc(50% + 105px);
          }
          100% {
            left: calc(50% + 105px);
          }
        }
        @keyframes dot-floating-before {
          0% {
            left: -50px;
          }
          50% {
            left: -12px;
          }
          75% {
            left: -50px;
          }
          100% {
            left: -50px;
          }
        }
        @keyframes dot-floating-after {
          0% {
            left: -100px;
          }
          50% {
            left: -24px;
          }
          75% {
            left: -100px;
          }
          100% {
            left: -100px;
          }
        }"
      ))
  ),
  
  # Main screen
  div(id = "main-content",
    titlePanel(div(a(href="https://hereon.de/index.php.en",
                     target="_blank",
                     img(src = "hereon-logo.svg", 
                         height = 78, 
                         width = 78, 
                         class = "pull-right",
                         style = "margin-top: -16px;")
                     ),
                   img(src = "scilitminer-logo.png", 
                       height = 54, 
                       width = 54, 
                       class = "pull-left"),
                   span("Scientific Literature Mining Platform",
                   style = "padding-left:10px;transition:translateY(-50%);font-family:'Audiowide',sans-serif;font-size:1.6em;text-transform:uppercase;letter-spacing:0.1px;background:linear-gradient(#020202,#848484,#020202);-webkit-background-clip:text;color:transparent;")
               ),
               windowTitle = "SciLitMiner"
    ),
    navbarPage(
      title = "MENU",
      id = "tabs",
      selected = "login_element",
      # write JS function to open imprint
      navbarMenu(title = "Privacy & Legal Notices",
                 tabPanel(a("Imprint", href = "https://www.hereon.de/innovation_transfer/communication_media/imprint/index.php.en", target = "_blank")),
                 tabPanel(a("Privacy & Data Protection", href = "https://www.hereon.de/innovation_transfer/communication_media/imprint/privacy_policy/index.php.en", target = "_blank"))
      ),
      tabPanel(title = "Login",
               value = "login_element",
               tags$br(),
               tags$br(),
               tags$br(),
               tags$br(),
               div(uiOutput("authreg"),
                   style = "display:flex;position:relative;justify-content:center;"
               )
      )
    )
  )
)


#Roboto Sarif, Montserrat, Titillium, Avenir, Playfair, Proxima Nova, Gotham
#Lato, Inter, Roboto Mono, Roboto COndensed, Barlow, Helvetica Neue, Avenir