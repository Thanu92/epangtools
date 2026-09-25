# epangtools

An R package for extracting and summarizing EPA-ng placements from jplace files.

## Installation

Install from GitHub:

install.packages("remotes")
remotes::install_github("YOUR_GITHUB_USERNAME/epangtools")

## Usage

library(epangtools)

results <- get_best_epang_placements(
"epa_result.jplace"
)

head(results)

## Author

M. A. Thanuja M. Fernando

## License

MIT
