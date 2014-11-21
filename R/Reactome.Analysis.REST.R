buildPostURL<-function(pageSize=1,page=1,sortBy="ENTITIES_PVALUE",resource="UNIPROT",projection) {
  projectionStr<-""
  if(projection) {
    projectionStr<-"/projection"
  }
  URLencode(paste("http://www.reactome.org/AnalysisService/identifiers/form",projectionStr,"?pageSize=",pageSize,"&page=",page,
        "&sortBy=",sortBy,"&order=ASC&resource=",resource,sep=""))
}

buildPathwaysDataDownloadURL<-function(resource="UNIPROT",token) {
  URLencode(paste("http://www.reactome.org/AnalysisService/download/",token,"/pathways/",resource,"/result.csv",sep=""))
}

downloadREACTOMEPathwayEnrichmentRes<-function(token,resource="UNIPROT") {
  getURL(url = buildPathwaysDataDownloadURL(token = token, resource = resource))->download
  textConnection(download)->con
  read.csv(con)
}

#' REACTOME Enrichment Analysis
#'
#' Sends a request to REACTOME to perform a simple enrichment analysis with the provided
#' identifiers.
#' 
#' @param identifiers The identifiers of biological entities for which an enrichment analysis
#' is requested.
#' @param resource The name of the resource to which the provided identifiers belong to. Can be any of
#' "UNIPROT", "TOTAL", "ENSEMBL", "CHEBI", "NCBI_PROTEIN", "EMBL" or "COMPOUND". Defaults to "UNIPROT".
#' @param projection Boolean, \code{TRUE} implies that orthology tables in REACTOME should be used to map the given identifiers
#' from their original species to human. This option allows to make better use of the resource as human pathways receive a lot more
#' attention in Reactome. Defaults to \code{FALSE}
#' @param getToken Boolean, \code{TRUE} implies the token for the analysis should be returned instead of a data.frame containing
#' the results. Through the token and direct REST calls, the user can obtain more data about the enrichment analysis.
#' @param token A token obtained from a previously done analysis (tokens can expire after 7 days). Using a token will ignore 
#' any identifiers given.
#' 
#' @return a data.frame with the result of the enrichment analysis. If "getToken" is set to true, the token of the
#' analysis is retrieved instead.
#' 
#' @examples
#' 
#' prots<-c()
REACTOMEEnrichmentAnalysis<-function(identifiers,resource="UNIPROT",projection=FALSE,getToken=FALSE,token=FALSE) {
  if(token) {
    return(downloadREACTOMEPathwayEnrichmentRes(token=token,resource=resource))
  }
  # we add a first line with a comments as with some initial proteins, the webservice responds with a Error: Unsupported Media Type
  # This doesn't happen when using the webservice through their documentation page, so it might be an issue with RCurl.
  # Offending protein was "P20701". Moving the protein to another position of the array fixed the problem.
  fileUpload(contents = as.character(paste(c("# First line comment",identifiers),collapse = "\n")), contentType = 'text/plain', filename = "proteins.txt")->fu
  postForm(uri = buildPostURL(resource = resource, projection = projection), file = fu)->jsonResp
  fromJSON(jsonResp,asText=TRUE)->parsedResp
  if(getToken) {
    return(parsedResp$summary$token)
  }
  downloadREACTOMEPathwayEnrichmentRes(token = parsedResp$summary$token, resource = resource)
}