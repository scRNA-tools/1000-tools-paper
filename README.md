
# 1000 Tools in the scRNA-seq analysis landscape

<!-- badges: start -->
<!-- badges: end -->

## Setting up

### Crossref

Information about publications and preprints is retrieved from the Crossref API using the **{rcrossref}** package.
As explained in `` ?rcrossref::`rcrossref-package` `` Crossref provides faster access to people who give an email address.
To do this add the following line to your `.Renviron`:

```
crossref_email=your@email.com
```

### Analytics

Data for plots showing usage statistics of the scRNA-tools website are collected using the **{googleAnalyticsR}** website.
For this to work you must set up authentication with the `googleAnalyticsR::ga_auth_setup()` function following the instructions here https://code.markedmondson.me/googleAnalyticsR/articles/setup.html.

> **NOTE:** Website analytics access requires being added to the Google Analytics group so will not work for most people.

At the end of the process your `.Renviron` file should contain lines similar to these:

```
GAR_CLIENT_JSON=/path/to/oauth.json
GARGLE_EMAIL=your@email.com
```
