# RIPPLE/FGV 2019 Project

## Crypto Diversifier

The objective of the app is to assist the user in the decision-making process regarding asset allocation. However, it does not constitute, under whichever circumstances, any investment recommendation or advice on our part.

Any information or analysis provided in this app comes from the authors’ alone and do not represent the view or has the consent of Ripple or FGV EESP. The accuracy, completeness, and validity of any data or analysis within this app are not guaranteed. We accept no liability for any errors, omissions, or representations.

---

### Dockerfile

In the directory of the project, there is a `Dockerfile` which was developed to ensure reproducibility of the deployment of this application. The first time building may take a while, since it has to install every R package used by the app. We built this `Dockerfile` above the __rocker/shiny:3.6.1__ image.

Since this deploy (with Dockerfile) is based on a Shiny Server, the default port mapped to the Docker Container is TCP/3838. Deployment with other technology stacks is possible, but the R version, and package snapshots must be referring to R 3.6.1, otherwise we cannot ensure the reproducibility of the analysis.

To run this app individually, the user must be at the root directory of this project and run the following commands:

```shell
docker build -t crypto_diversifier .
docker run --name crypto_diversifier_container -p 3838:3838 -d crypto_diversifier
```

This will run the app and host it at [localhost](http://localhost:3838/diversified_crypto)
