# Executable Environment for OSF Project [7v4ep](https://osf.io/7v4ep/)

This repository was automatically generated as part of a project to test the reproducibility of open science projects hosted on the Open Science Framework (OSF).

**Project Title:** Data and R code for: "Collaboration enhances career progression in academic science, especially for female researchers"

**Project Description:**
> Data and code used for: van der Wal et al. (2021) "Collaboration enhances career progression in academic science, especially for female researchers" Proc. B.

PAPER ABSTRACT
Collaboration and diversity are increasingly promoted in science. Yet how collaborations influence academic career progression, and whether this differs by gender, remains largely unknown. Here we use co-authorship egonetworks to quantify collaboration behaviour and career progression of a cohort of contributors to biennial International Society of Behavioral Ecology meetings (1992, 1994, 1996). Among this cohort, women were slower and less likely to become a principal investigator (PI; approximated by having at least three last-author publications), and published fewer papers over fewer years (i.e. had shorter academic careers) than men. After adjusting for publication number, women also had fewer collaborators (lower adjusted network size) and published fewer times with each co-author (lower adjusted tie strength), albeit more often with the same group of collaborators (higher adjusted clustering coefficient). Authors with stronger networks were more likely to become a PI, and those with less clustered networks did so more quickly. Women, however, showed a stronger positive relationship with adjusted network size (increased career length) and adjusted tie strength (increased likelihood to become a PI). Finally, early-career network characteristics correlated with career length. Our results suggest that large and varied collaboration networks are positively correlated with career progression, especially for women.

**Original OSF Page:** [https://osf.io/7v4ep/](https://osf.io/7v4ep/)

---

**Important Note:** The contents of the `7v4ep_src` folder were cloned from the OSF project on **12-03-2025**. Any changes made to the original OSF project after this date will not be reflected in this repository.

The `DESCRIPTION` file was automatically added to make this project Binder-ready. For more information on how R-based OSF projects are containerized, please refer to the `osf-to-binder` GitHub repository: [https://github.com/Code-Inspect/osf-to-binder](https://github.com/Code-Inspect/osf-to-binder)

## flowR Integration

This version of the repository has the **[flowR Addin](https://github.com/flowr-analysis/rstudio-addin-flowr)** preinstalled. flowR allows visual design and execution of data analysis workflows within RStudio, supporting better reproducibility and modular analysis pipelines.

To use flowR, open the project in RStudio and go to `Addins` > `flowR`.

## How to Launch:

**Launch in your Browser:**

🚀 **MyBinder:** [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/code-inspect-binder/osf_7v4ep-f/HEAD?urlpath=rstudio)

   * This will launch the project in an interactive RStudio environment in your web browser.
   * Please note that Binder may take a few minutes to build the environment.

🚀 **NFDI JupyterHub:** [![NFDI](https://nfdi-jupyter.de/images/nfdi_badge.svg)](https://hub.nfdi-jupyter.de/r2d/gh/code-inspect-binder/osf_7v4ep-f/HEAD?urlpath=rstudio)

   * This will launch the project in an interactive RStudio environment on the NFDI JupyterHub platform.

**Access Downloaded Data:**
The downloaded data from the OSF project is located in the `7v4ep_src` folder.

## Run via Docker for Long-Term Reproducibility

In addition to launching this project using Binder or NFDI JupyterHub, you can reproduce the environment locally using Docker. This is especially useful for long-term access, offline use, or high-performance computing environments.

### Pull the Docker Image

```bash
docker pull meet261/repo2docker-7v4ep-f:latest
```

### Launch RStudio Server

Run the container (with a name, e.g. `rstudio-dev`):
```bash
docker run -it --name rstudio-dev --platform linux/amd64 -p 8888:8787 --user root meet261/repo2docker-7v4ep-f bash
```

Inside the container, start RStudio Server with no authentication:
```bash
/usr/lib/rstudio-server/bin/rserver --www-port 8787 --auth-none=1
```

Then, open your browser and go to: [http://localhost:8888](http://localhost:8888)

> **Note:** If you're running the container on a remote server (e.g., via SSH), replace `localhost` with your server's IP address.
> For example: `http://<your-server-ip>:8888`

## Looking for the Base Version?

For the original Binder-ready repository **without flowR**, visit:
[osf_7v4ep](https://github.com/code-inspect-binder/osf_7v4ep)

