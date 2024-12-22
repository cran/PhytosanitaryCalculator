\name{PhytosanitaryCalculator-package}
\alias{PhytosanitaryCalculator-package}
\alias{PhytosanitaryCalculator}
\docType{package}
\title{
Phytosanitary Calculator and Inspection Sampling Planner
}
\author{
Gustavo Ramirez-Valverde, Luis Gabriel Otero-Prevost, Pedro Macias-Canales,
Juan A. Villanueva-Jimenez, Jorge Luis Leyva-Vazquez
Maintainer: Gustavo Ramirez-Valverde <gustavoramirezvalverde@gmail.com>
}
\description{
The package is a Shiny application designed to simplify and automate the planning of phytosanitary inspections. Its key features include:
- **Phytosanitary Inspection Plan**: Automatically calculates optimized sampling plans with minimal sample sizes, ensuring compliance with user-defined risk thresholds for both producers and importers.

- **Phytosanitary Sampling Design for Inspection Plans**: Facilitates the design of sampling plans for phytosanitary inspections, particularly for products organized in batches or pallets. The application generates clear visual diagrams to identify sampled units and thoroughly documents the selection process for better traceability.
}
\details{
The package is designed for applications in phytosanitary inspections at customs, quality control within agricultural supply chains, and other industries requiring inspection and control at the batch or lot level.

**Key Features:**
- **Phytosanitary Inspection Plan**:
  The package calculates minimal sample sizes required to meet user-defined risk thresholds for producers and importers, based on predefined Acceptable Sanity Level and Limit Sanity Level parameters. These calculations employ established statistical methods for acceptance sampling and quality control, incorporating Poisson, hypergeometric, and binomial distribution models to address various inspection scenarios and lot characteristics.

- **Phytosanitary Sampling Design for inspection plans**:
  Using the results of the Phytosanitary Inspection Plan, the package constructs a tailored sampling design for inspection purposes. It assumes that products are arranged in batches or pallets and generates visual diagrams to identify the sampled units while documenting the selection process. The package supports both simple random sampling and systematic sampling with a random start, offering flexibility to adapt to different protocols. Users can customize pallet layouts, including the number of rows, columns, and height, to accommodate various inspection scenarios. Additionally, the package produces HTML summary reports that are ready for printing, ensuring clear communication of sampling plans and compliance with phytosanitary inspection standards.
}
\references{
Guenther, W. C. (1977). Sampling Inspection in Statistical Quality Control. Charles Griffin and Company Limited.

Hald, A. (1981). Statistical Theory of Sampling Inspection by Attributes. Academic Press.(ISBN 9780123183507)

Schilling, E. G. (1982). Acceptance Sampling in Quality Control. Dekker. (ISBN: 9780429145636)

Lohr, S. (2019). Sampling, second edition. Chapman and Hall/CRC. (ISBN: 9780367273415)

Cochran, W. G. (1977). Sampling Techniques, third edition. Wiley. (ISBN: 9780471162407)
}
\examples{
if (interactive()) {
  run_app()
}
}
\keyword{package}
