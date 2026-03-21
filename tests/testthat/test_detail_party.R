# Tests for party detail view

test_that("build_party_details_view handles NULL data gracefully", {
  # When NULL data is provided
  result <- build_party_details_view(NULL)

  # It should return a list with placeholder components
  expect_type(result, "list")
  expect_setequal(names(result), c(
    "party_header", "party_organization", "party_contact", "party_contributions"
  ))

  # Each component should be a render function
  expect_true(inherits(result$party_header, "shiny.render.function"))
  expect_true(inherits(result$party_organization, "shiny.render.function"))
  expect_true(inherits(result$party_contact, "shiny.render.function"))
  expect_true(inherits(result$party_contributions, "shiny.render.function"))
})

test_that("build_party_details_view formats party data correctly — all three mocks", {
  for (mock in list(mock_party_py415, mock_party_py17, mock_party_py199146)) {
    result <- build_party_details_view(mock)
    expect_type(result, "list")
    expect_setequal(names(result), c(
      "party_header", "party_organization", "party_contact", "party_contributions"
    ))
    expect_true(inherits(result$party_header, "shiny.render.function"))
    expect_true(inherits(result$party_organization, "shiny.render.function"))
    expect_true(inherits(result$party_contact, "shiny.render.function"))
    expect_true(inherits(result$party_contributions, "shiny.render.function"))
  }
})

test_that("build_party_details_view renders correct header content", {
  # py.415: Jennings, Michael — no salutation, no middle name
  shiny::testServer(
    function(input, output, session) {
      output$test <- build_party_details_view(mock_party_py415)$party_header
    },
    {
      html <- session$getOutput("test")
      expect_true(grepl("Jennings, Michael", html$html))    # label
      expect_true(grepl("Michael Jennings", html$html))     # full_name (given + surname)
      expect_true(grepl("py.415", html$html))               # vb_code
    }
  )

  # py.17: Drake, Jim — has salutation "Mr."
  shiny::testServer(
    function(input, output, session) {
      output$test <- build_party_details_view(mock_party_py17)$party_header
    },
    {
      html <- session$getOutput("test")
      expect_true(grepl("Drake, Jim", html$html))           # label
      expect_true(grepl("Mr. Jim Drake", html$html))        # full_name (salutation + given + surname)
      expect_true(grepl("py.17", html$html))                # vb_code
    }
  )

  # py.199146: Heaney, Mike — no salutation, no middle, no org
  shiny::testServer(
    function(input, output, session) {
      output$test <- build_party_details_view(mock_party_py199146)$party_header
    },
    {
      html <- session$getOutput("test")
      expect_true(grepl("Heaney, Mike", html$html))         # label
      expect_true(grepl("Mike Heaney", html$html))          # full_name
      expect_true(grepl("py.199146", html$html))            # vb_code
    }
  )
})

test_that("build_party_details_view renders correct organization content", {
  # py.415: has organization
  shiny::testServer(
    function(input, output, session) {
      output$test <- build_party_details_view(mock_party_py415)$party_organization
    },
    {
      html <- session$getOutput("test")
      expect_true(grepl("University of Idaho, Department of Geography", html$html))
    }
  )

  # py.17: The Nature Conservancy
  shiny::testServer(
    function(input, output, session) {
      output$test <- build_party_details_view(mock_party_py17)$party_organization
    },
    {
      html <- session$getOutput("test")
      expect_true(grepl("The Nature Conservancy", html$html))
    }
  )

  # py.199146: NA organization → fallback text
  shiny::testServer(
    function(input, output, session) {
      output$test <- build_party_details_view(mock_party_py199146)$party_organization
    },
    {
      html <- session$getOutput("test")
      expect_true(grepl("No organization recorded", html$html))
    }
  )
})

test_that("build_party_details_view renders correct contact content", {
  # py.415: NA contact_instructions → fallback text
  shiny::testServer(
    function(input, output, session) {
      output$test <- build_party_details_view(mock_party_py415)$party_contact
    },
    {
      html <- session$getOutput("test")
      expect_true(grepl("No contact information recorded", html$html))
    }
  )

  # py.17: has contact instructions
  shiny::testServer(
    function(input, output, session) {
      output$test <- build_party_details_view(mock_party_py17)$party_contact
    },
    {
      html <- session$getOutput("test")
      expect_true(grepl("contact the contributor at specified email:", html$html))
    }
  )

  # py.199146: NA → fallback
  shiny::testServer(
    function(input, output, session) {
      output$test <- build_party_details_view(mock_party_py199146)$party_contact
    },
    {
      html <- session$getOutput("test")
      expect_true(grepl("No contact information recorded", html$html))
    }
  )
})

test_that("build_party_details_view renders correct contributions content", {
  # py.415: 8657 observations → clickable link
  shiny::testServer(
    function(input, output, session) {
      output$test <- build_party_details_view(mock_party_py415)$party_contributions
    },
    {
      html <- session$getOutput("test")
      expect_true(grepl("8657", html$html))
      expect_true(grepl("obs-count-link", html$html))
    }
  )

  # py.17: 290 observations → clickable link
  shiny::testServer(
    function(input, output, session) {
      output$test <- build_party_details_view(mock_party_py17)$party_contributions
    },
    {
      html <- session$getOutput("test")
      expect_true(grepl("290", html$html))
      expect_true(grepl("obs-count-link", html$html))
    }
  )

  # py.199146: NA obs_count → 0 → span with "0", no link
  shiny::testServer(
    function(input, output, session) {
      output$test <- build_party_details_view(mock_party_py199146)$party_contributions
    },
    {
      html <- session$getOutput("test")
      expect_true(grepl(">0<", html$html))
      expect_false(grepl("obs-count-link", html$html))
    }
  )
})

