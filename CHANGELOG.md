# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.2.0] - 2020-08-31

### Added
- Integrate with Jason Gross' coq-bug-minimizer tool.
- Merge a branch in the coq repository if some conditions are met, by writing `@coqbot: merge now` in a comment.
- Parametrize the bot with a configuration file.
- Installation as a GitHub App is supported.
- Report CI status checks with the Checks API when using the GitHub app.
- Report errors of jobs in allow failure mode when the Checks API is used.

### Changed
- Refactored the architecture of the application and of the bot-components library
- More informative bot merge commit title for GitLab CI.

## [0.1.0] - 2020-07-09
Initial release of coqbot.

### Added
- Push new and updated pull requests to branches on a GitLab mirror.
- Automatic merge commit for pull requests that aren't up-to-date with respect to the base branch. 
- Push a failed status check on a PR if the automatic merge fails, and set a "needs: rebase" label
(removes the label once an updated version without conflicts is pushed).
- Detailed status check reporting of failing pipelines from GitLab CI
(with direct links to the failed jobs from within the PR in GitHub).
- Delete branches corresponding to pull requests when the pull requests are merged or closed.
- Clear milestone of unmerged pull requests.
- Synchronize closed issue's milestone with the one of the pull request that closed it.
- Post comment when a pull request does not respect certain standards.
- Manage the backporting process.
- Automatic release, build and deployment of Docker images of the bot to Heroku and GitHub packages.

[Unreleased]: https://github.com/coq/bot/compare/v0.1.0...HEAD
[0.1.0]: https://github.com/coq/bot/releases/tag/v0.1.0
