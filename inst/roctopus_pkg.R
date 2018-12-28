devtools::install_bitbucket()
remotes::download_method()

# Create hex sticker for the package ####
install.packages("hexSticker")
library(hexSticker)
sticker("inst/octopus-art-tako.jpg",
        s_x=1, s_y=1, s_width=.55, s_height=.5,
        h_fill = "#FDF4D3", h_color = "#95B2BA",
        package="rOctopus", p_size=8, p_color = "#95B2BA",
        filename="man/figures/logo.png")

# Define roles ####
library(devtools)
library(desc)
desc_del("Authors@R")
desc_add_author("James","Hollway", email="james.hollway@graduateinstitute.ch",
                role="cre", orcid="0000-0002-8361-9647", comment = "IHEID")
desc_add_author("Valentina","Baiamonte", role = "ctb", comment = "IHEID")
desc_add_role("aut","James","Hollway")
desc_add_role("ctb","James","Hollway")
desc_bump_version

# Make website ####
library(pkgdown)
build_favicon()
use_lifecycle_badge("Experimental")
build_site(run_dont_run = T)
preview_site()
use_news_md()
