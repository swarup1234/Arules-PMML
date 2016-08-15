
read_pmml_custom<-function (model, model.name = "arules_Model", app.name = "Rattle/PMML", 
          description = "arules association rules model", copyright = NULL, 
          transforms = NULL, ...) {
  if (!inherits(model, "rules")) 
    stop("Not a legitimate arules rules object")
  requireNamespace("arules", quietly = TRUE)
  pmml <- pmmlRootNode("4.1")
  pmml <- append.XMLNode(pmml, pmmlHeader(description, copyright, 
                                           app.name))
  data.dictionary <- xmlNode("DataDictionary", attrs = c(numberOfFields = 2L))
  data.dictionary <- append.xmlNode(data.dictionary, list(xmlNode("DataField", 
                                                                  attrs = c(name = "transaction", optype = "categorical", 
                                                                            dataType = "string")), xmlNode("DataField", attrs = c(name = "item", 
                                                                                                                                  optype = "categorical", dataType = "string"))))
  pmml <- append.XMLNode(pmml, data.dictionary)
  quality <- quality(model)
  is <- c(arules::lhs(model), arules::rhs(model))
  is.unique <- getMethod("unique", "itemMatrix")(is)
  association.model <- xmlNode("AssociationModel", attrs = c(functionName = "associationRules", 
                                                             numberOfTransactions = arules::info(model)$ntransactions, 
                                                             numberOfItems = length(arules::itemLabels(model)), minimumSupport = arules::info(model)$support, 
                                                             minimumConfidence = arules::info(model)$confidence, numberOfItemsets = length(is.unique), 
                                                             numberOfRules = length(model)))
  mining.schema <- xmlNode("MiningSchema")
  mining.schema <- append.xmlNode(mining.schema, list(xmlNode("MiningField", 
                                                              attrs = c(name = "transaction", usageType = "group")), 
                                                      xmlNode("MiningField", attrs = c(name = "item", usageType = "active"))))
  association.model <- append.xmlNode(association.model, mining.schema)
  #items <- list()
  il<-itemLabels(model)
  il <- markupSpecials(arules::itemLabels(model))
  #system.time(for (i in 1:length(il)) items[[i]] <- xmlNode("Item", attrs = list(id = i, 
  #                                                                   value = il[i])))
  cat(" creating items...")
  itm<-lapply(1:length(il),FUN = function(x)xmlNode("Item", attrs = list(id = x,value = il[x])))
  association.model <- append.xmlNode(association.model, itm)
  print(object.size(itm)/(1024*1024))
  rm(itm)
  #itemsets <- list()
  sizes <- arules::size(is.unique)
  isl <- arules::LIST(is.unique, decode = FALSE)
  # for (i in 1:length(isl)) {
  #   itemsets[[i]] <- xmlNode("Itemset", attrs = list(id = i,
  #                                                    numberOfItems = sizes[i]))
  #   items <- list()
  #   if (sizes[i] > 0)
  #     for (j in 1:sizes[i]) items[[j]] <- xmlNode("ItemRef",
  #                                                 attrs = list(itemRef = isl[[i]][j]))
  #   itemsets[[i]] <- append.xmlNode(itemsets[[i]], items)
  # }
  # system.time(itemsets1<-lapply(1:length(isl),function(x){
  #   is1<-xmlNode("Itemset", attrs = list(id = x, numberOfItems = sizes[x]))
  #   items <- list()
  #   if (sizes[x] > 0)
  #   for (j in 1:sizes[x]) items[[j]] <- xmlNode("ItemRef",
  #                                               attrs = list(itemRef = isl[[x]][j]))
  #   #items<-lapply(1:sizes[t],xmlNode("ItemRef", attrs = list(itemRef = isl[[t]][x])))
  #   is1<-append.xmlNode(is1,items)
  #   return(is1)
  # }))
  cat("\n creating itemsets ...")
  itemsets<-lapply(1:length(isl),function(x){
    is1<-xmlNode("Itemset", attrs = list(id = x, numberOfItems = sizes[x]))
    t=x
    if (sizes[t] > 0) 
      items<-lapply(1:sizes[t],function(y)xmlNode("ItemRef", attrs = list(itemRef = isl[[t]][y])))
    is1<-append.xmlNode(is1,items)
    return(is1)
  })
  association.model <- append.xmlNode(association.model, itemsets)
  print(object.size(itemsets)/(1024*1024))
  rm(itemsets)
  mlhs <- getMethod("match", c("itemMatrix", "itemMatrix"))(arules::lhs(model), 
                                                            is.unique)
  mrhs <- getMethod("match", c("itemMatrix", "itemMatrix"))(arules::rhs(model), 
                                                            is.unique)
  # rules <- list()
  # for (i in 1:length(model)) {
  #   rules[[i]] <- xmlNode("AssociationRule", attrs = list(support = quality$support[i], 
  #                                                         confidence = quality$confidence[i], lift = quality$lift[i], 
  #                                                         antecedent = mlhs[i], consequent = mrhs[i]))
  # }
  # 
  cat("\n creating rule details...")
  rules <- lapply(1:length(model),function(x)xmlNode("AssociationRule", attrs = list(support = quality$support[x], 
                                                          confidence = quality$confidence[x], lift = quality$lift[x], 
                                                          antecedent = mlhs[x], consequent = mrhs[x])))
  
  association.model <- append.xmlNode(association.model, rules)
  print(object.size(rules)/(1024*1024))
  rm(rules)
  
  cat("\n creating the final pmml file...")
  pmml <- append.XMLNode(pmml, association.model)
  print(object.size(pmml)/(1024*1024))
  rm(association.model)
  return(pmml)
}

pmmlHeader <- function(description, copyright, app.name)
{
  # Header
  
  VERSION <- "1.2.34"
  DATE <- "2013-02-02"
  REVISION <- "2"
  
  if (is.null(copyright)) copyright <- generateCopyright()
  header <- xmlNode("Header",
                    attrs=c(copyright=copyright, description=description))
  
  # Header -> User (Extension)
  #
  # 100519 wenching.lin@zementis.com pointed out that the DMG spec
  # requires the Extension to be first.
  
  header <- append.XMLNode(header, xmlNode("Extension",
                                           attrs=c(name="user",
                                                   value=sprintf("%s",
                                                                 Sys.info()["user"]),
                                                   extender=app.name)))
  
  # Header -> Application
  
  header <- append.XMLNode(header, xmlNode("Application",
                                           attrs=c(name=app.name,
                                                   version=paste(VERSION, "r", REVISION, sep=""))))
  
  # Header -> Timestamp
  
  header <- append.XMLNode(header,
                           xmlNode("Timestamp", sprintf("%s", Sys.time())))
  
  return(header)
}

pmmlRootNode <- function(version)
{
  # 080621 Don't use a default since we should specify in the call
  # which version the code is targetting - for documentation purposes.
  
  ##  PMML.VERSION <- "3.1" 080621 Did I user this string anywhere for any
  ##  external purpose. Maybe not - so it should be removed.
  
  # 080615 - The xmlns= namespace addition causes a warning on calling
  # getNodeSet unless we specify the namespace there:
  # 	> doc <- xmlTreeParse("test.xml", useInternalNodes=TRUE)
  #     > els <- getNodeSet(doc, "/PMML/DataDictionary/DataField")
  #     Warning message:
  #     using http://www.dmg.org/PMML-3_1 as prefix for default
  #     namespace http://www.dmg.org/PMML-3_1 
  #     > els <- getNodeSet(doc, "//p:DataField", c(p="http://www.dmg.org/PMML-3_1"))
  # We are supposed to include a default namespace, so do so. 
  
  if (version == "3.1")
    node <- xmlNode("PMML",
                    attrs=c(version="3.1",
                            xmlns="http://www.dmg.org/PMML-3_1",
                            "xmlns:xsi"="http://www.w3.org/2001/XMLSchema-instance"))
  else if (version == "3.2")
    node <- xmlNode("PMML",
                    attrs=c(version="3.2",
                            xmlns="http://www.dmg.org/PMML-3_2",
                            "xmlns:xsi"="http://www.w3.org/2001/XMLSchema-instance", 
                            "xsi:schemaLocation"=paste("http://www.dmg.org/PMML-3_2",
                                                       "http://www.dmg.org/v3-2/pmml-3-2.xsd")))
  else if (version == "4.1")
    node <- xmlNode("PMML",
                    attrs=c(version="4.1",
                            xmlns="http://www.dmg.org/PMML-4_1",
                            "xmlns:xsi"="http://www.w3.org/2001/XMLSchema-instance", 
                            "xsi:schemaLocation"=paste("http://www.dmg.org/PMML-4_1",
                                                       "http://www.dmg.org/v4-1/pmml-4-1.xsd")))
  else
    node <- xmlNode("PMML",
                    attrs=c(version="4.0",
                            xmlns="http://www.dmg.org/PMML-4_0",
                            "xmlns:xsi"="http://www.w3.org/2001/XMLSchema-instance",
                            "xsi:schemaLocation"=paste("http://www.dmg.org/PMML-4_0",
                                                       "http://www.dmg.org/v4-0/pmml-4-0.xsd")))
  
  return(node)
}

generateCopyright <- function()
{
  return(paste("Copyright (c)", format(Sys.time(), "%Y"), Sys.info()["user"]))
}

markupSpecials <- function(x)
  gsub("<", "&lt;", gsub(">", "&gt;", gsub("&", "&amp;", x)))