#' Access All Available Petfinder Data
#' @description This function allows the users to access pet adoption data from Petfinder 2.0 API
#' through a variety of different parameters. Parameters should be left null if it is not of interest
#' so as to include all fields that have that parameter.
#' @param breed Accepts multiple values, e.g. breed=pug,samoyed.
#' @param size small, medium, large, xlarge Accepts multiple values, e.g. size=large,xlarge.
#' @param gender male, female, unknown Accepts multiple values, e.g. gender=male,female.
#' @param age baby, young, adult, senior Accepts multiple values, e.g. age=baby,senior.
#' @param color Return results matching animal color
#' @param coat Return results matching animal coat
#' @param status Return results matching adoption status
#' @param organization Return results associated with specific organization(s)
#' @param good_with_children Return results that are good with children. Can be true, false, 1, or 0
#' @param good_with_dogs Return results that are good with dogs. Can be true, false, 1, or 0
#' @param good_with_cats Return results that are good with cats. Can be true, false, 1, or 0
#' @param location Return results by location. string city, state; latitude,longitude; or postal code.
#' @param distance Return results within distance of location (in miles). Requires location to be set (default: 100, max: 500)
#' @param before Return results published before this date/time. Must be a valid ISO8601 date-time string (e.g. 2019-10-07T19:13:01+00:00)
#' @param after Return results published after this date/time. Must be a valid ISO8601 date-time string (e.g. 2019-10-07T19:13:01+00:00)
#' @param limit Sets the number of observations to return. Default is 20.
#' @examples
#' GetAnimals()
#'
#' @details
#' Returns a dataframe with all available features for 20 observations.
#' Parameters will filter based on selected values and number of observations can be adjusted with
#' the limit parameter.
#' @export

GetAnimals <- function(breed = NULL, size = NULL, gender = NULL, age = NULL, color = NULL,
                       coat = NULL, status = NULL, organization = NULL, good_with_children = NULL,
                       good_with_dogs = NULL, good_with_cats = NULL, location = NULL,
                       distance = NULL, before = NULL, after = NULL, limit = 20)
{
  #set authentication parameters to retrieve token
  oauth_petfinder_endpoint <- httr::oauth_endpoint(access="https://api.petfinder.com/v2/oauth2/token",
                                                   authorize = NULL)
  petfinder_app <- httr::oauth_app('petfinder',
                                   key = PETFINDER_APP_KEY,
                                   secret = PETFINDER_APP_SECRET)
  petfinder_token <- httr::oauth2.0_token(endpoint = oauth_petfinder_endpoint,
                                          app = petfinder_app,
                                          client_credentials = T)
  #set user
  user <- httr::user_agent("R User")
  #url query
  url <- httr::modify_url(
    url = "https://api.petfinder.com",
    path = c("v2", "animals"),
    query = list(
      breed = breed,
      size = size,
      gender = gender,
      age = age,
      color = color,
      coat = coat,
      status = status,
      organization = organization,
      good_with_children = good_with_children,
      good_with_dogs = good_with_dogs,
      good_with_cats = good_with_cats,
      location = location,
      distance = distance,
      before = before,
      after = after,
      limit = limit
    )

  )
  #request with custom R user and token
  parameter_response <- httr::GET(url, user, config(token = petfinder_token))

  if(parameter_response$status_code == 200)
  {
    content <- jsonlite::fromJSON(httr::content(parameter_response, "text", encoding = 'UTF-8'))
  }
  else
  {
    print("Error with request, please check query")
  }
}
