# Make a system prompt based on selected Task
make_system_prompt <- function(chat_task = c("general", "code")) {
  system_content <- switch(
    chat_task,
    "general" = "You are a helpful assistant.",
    "code" = paste("You are a helpful chat bot that answers questions ",
                   "to an R programmer working in the RStudio IDE.")
  )
  list(list(role = "system", content = system_content))
}

# Make a user prompt based on input text.
make_user_prompt <- function(user_content) {
  list(list(role = "user", content = user_content))
}

chat <- function(user_content = NULL,
                 chat_history = NULL,
                 chat_model = c("gpt-3.5-turbo", "gpt-4"),
                 chat_task = c("general", "code"),
                 api_key = "No code entered"
                 ) {

  chat_messages <- c(
    make_system_prompt(chat_task),
    chat_history,
    make_user_prompt(user_content)
  ) |> purrr::compact()

  api_key = if_else(api_key == Sys.getenv("FOLEY_KEY"),
                    Sys.getenv("OPENAI_API_KEY"),
                    api_key)

  resp <-
    request("https://api.openai.com/v1") |>
    req_url_path_append("chat/completions") |>
    req_auth_bearer_token(token = api_key) |>
    req_headers("Content-Type" = "application/json") |>
    req_user_agent("Mike Foley @mpfoley73 | Toy Project") |>
    req_body_json(data = list(model = chat_model, messages = chat_messages)) |>
    req_retry(max_tries = 4) |>
    req_throttle(rate = 15) |>
    req_perform()

  openai_chat_response <- resp |> resp_body_json(simplifyVector = TRUE)

  openai_chat_response$choices$message$content
}

update_history <- function(chat_history, user_content, response) {
  c(chat_history,
    list(
      list(role = "user", content = user_content),
      list(role = "assistant", content = response)
    )) |>
    purrr::compact() |>
    rev()
}
