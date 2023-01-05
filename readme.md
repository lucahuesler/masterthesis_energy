# Master thesis: Modelling energy consumption in the canton of Basel-Landschaft

## Version control

Version control is done with github.

### Initial setup

For easy use of are and github, I use the `usethis` library

    install.packages("devtools")
    library(usethis)

Initial setup:

     usethis::create_github_token()
    • Call `gitcreds::gitcreds_set()` to register this token in the local Git credential store
      It is also a great idea to store this token in any password-management software that you use
    ✔ Opening URL 'https://github.com/settings/tokens/new?scopes=repo,user,gist,workflow&description=DESCRIBE THE TOKEN\'S USE CASE'
    > gitcreds::gitcreds_set()


    ? Enter password or token: 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
    -> Adding new credentials...
    -> Removing credentials from cache...
    -> Done.
    > gh::gh_whoami()
    {
      "name": "Luca Hüsler",
      "login": "lucahuesler",
      "html_url": "https://github.com/lucahuesler",
      "scopes": "gist, repo, user, workflow",
      "token": "ghp_...udtL"
    } 

After the initial setup, we can use github for our R projects.

Local setup:

    usethis::use_git()

For the remote git, we use:

    usethis::use_github()

## Reproducibility

For reproducibility, the `renv` package is used.
