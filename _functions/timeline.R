# ADAPTED FROM:
# Bruno Rodrigues
# https://github.com/itbruno
# Codepen: https://codepen.io/itbruno/pen/KwarLp

#' GOAL
#' Initialize the div container that contains the timeline
#' @return HTML
start_timeline <- function() {
  #' ORIGINAL HTML
  # <div class="container">
  # 		<div id="timeline">
  return(
    htmltools::HTML(
      glue::glue(
        "<div class = 'container'>",
        "<div id = 'timeline'>"
      ) # end glue
    ) # end html
  ) # end return
} # end start_timeline

#' GOAL
#' Close the div container that contains the timeline
#' @return HTML
close_timeline <- function() {
  #' ORIGINAL HTML
  # 				</div>
  # 			</div>
  # Close the containers
  return(
    htmltools::HTML(
      glue::glue(
        "</div> <!-- close timeline -->",
        "</div> <!-- close container -->",
      ) # end glue
    ) # end html
  ) # end return
} # end close_timeline

#' GOAL
#' Add a card entry to the timeline
#' @param title = title of the card (large and bold font)
#' @param description = text contained in the card.
#' ... Can include any html as desired (e.g., links)
#' @param publication_date = character string to include as the publication date.
#' ... defaults to today, but can be formatted according to preferences.
#' @param card_position = left/right = position of the card in relationship to the
#' ... timeline center
#' ... defaults to left, also left for small screen layouts
#' @param icon_name = a fontawesome icon name, for the available options
#' ... see https://fontawesome.com/v5.15/icons?d=gallery&p=2&m=free
#' @param icon_color = defaults to white, should be adjusted in line with other styling
#' @param button_text = a character string to display on the button.
#' ... Automatically converted to uppercase.
#' @param button_url = a link to add to the button, can be both local and external
#'
#' STYLING
#' For changes to the font and background colors see the CSS/timeline.css file
add_item <- function(title = NULL,
                     description = NULL,
                     publication_date = Sys.Date(),
                     card_position = "left",
                     icon_name = NULL,
                     icon_color = "#FFFFFF",
                     button_text = NULL,
                     button_url = NULL

) {

  #' ITEM CONTAINER
  #' 			<div class="timeline-item">
  item_container <-
    htmltools::div(
      "class" = "timeline-item"
    )

  #### ICON ####
  # Container element - necessary for styling
  # 				<div class="timeline-icon">
  icon_container <-
    htmltools::div(
      class = "timeline-icon"
    )

  # Icon
  # <svg ... >
  if (is.null(icon_name)) {
    icon_name <- "asterisk"
  }

  #   <svg version="1.1" id="Layer_1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0px" y="0px"
  # 	 width="21px" height="20px" viewBox="0 0 21 20" enable-background="new 0 0 21 20" xml:space="preserve">
  # Create the icon element with the fontawesome package
  # for available icon names see: https://fontawesome.com/v5.15/icons?d=gallery&p=2&m=free
  icon <-
    fontawesome::fa(
      name = icon_name,
      height = "2em",
      fill = icon_color
    )

  # Add icon to container
  icon_container <-
    htmltools::tagAppendChild(
      # parent
      icon_container,
      # child
      icon
    )

  # Add icon_container to item_container
  item_container <-
    htmltools::tagAppendChild(
      # parent
      item_container,
      # child
      icon_container
    )

  #### CONTENT ####
  # CONTAINER
  # 				<div class="timeline-content">
  content_container <-
    htmltools::div(
      class = ifelse(card_position == "left",
                     "timeline-content",
                     "timeline-content right"),
      # HEADER
      # 					<h2>LOREM IPSUM DOLOR</h2>
      htmltools::h3(
        title
      ),
      # DATE
      htmltools::h3(
        publication_date,
        class = "timeline-date"
      ),
      # CONTENT
      # 					<p>
      # 						Lorem ipsum dolor sit amet, consectetur adipisicing elit.
      # 						Atque, facilis quo maiores magnam modi ab libero praesentium blanditiis.
      # 					</p>
      #
      htmltools::p(
        htmltools::HTML(
          description
        )
      ),
    )

  # BUTTON
  #  					<a href="#" class="btn">button</a>
  if (!is.null(button_url) & !is.null(button_text)) {
    content_container <-
      htmltools::tagAppendChild(
        content_container,
        htmltools::a(
          button_text,
          class = "btn",
          href = button_url
        )
      )
  }

  # Add content_container to the item_container
  item_container <-
    htmltools::tagAppendChild(
      item_container,
      content_container
    )

  return(item_container)

}
