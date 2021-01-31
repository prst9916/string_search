# Document Search
The goal of this exercise is to create a working program to search a set of documents for the given search term or phrase (single token), and return results in order of relevance. Relevancy is defined as number of times the exact term or phrase appears in the document. Create three methods for searching the documents: Simple string matchingText search using regular expressionsPreprocess the content and then search the index.Prompt the user to enter a search term and search method, execute the search, and return results. 
For instance:Three files have been provided for you to read and use as sample search content.Run a performance test that does 2M searches with random search terms, and measures execution time. Which approach is fastest? Why?Provide some thoughts on what you would do on the software or hardware side to make this program scale to handle massive content and/or very large request volume (5000 requests/second or more). 
# Prerequisites
Make sure to fulfill the following requirements:
You have access to an SAP Cloud Platform ABAP Environment instance (see here for additional information).
You have downloaded and installed ABAP Development Tools (ADT). Make sure to use the most recent version as indicated on the installation page.
You have created an ABAP Cloud Project in ADT that allows you to access your SAP Cloud Platform ABAP Environment instance (see here for additional information).
You have installed the abapGit plug-in for ADT from the update site http://eclipse.abapgit.org/updatesite/.
# Download
Use the abapGit plug-in to install the Doc Search Scenario by executing the following steps:
In your ABAP cloud project, create the ABAP package Z_STRINGSEARCH
To add the abapGit Repositories view to the ABAP perspective, click Window > Show View > Other... from the menu bar and choose abapGit Repositories.
In the abapGit Repositories view, click the + icon to clone an abapGit repository.
Enter the following URL of this repository: https://github.com/prst9916/string_search.git and choose Next.
Select the master branch and enter the newly created package Z_STRINGSEARCH as the target package.
Create a new transport request that you only use for this demo content installation (recommendation) and choose Finish to start the cloning of the repository contents. Note that this procedure may take a few minutes.
Once the cloning has finished, refresh your project tree.
As a result of the installation procedure above, the ABAP system creates an inactive version of all artifacts from the demo content and adds the following objects to the target package:
Programs : 
i)  ZSTRING_SEARCH
ii) ZSO10_UPLOAD_PROGRAM
Tables : 
i) ZSO10_SRCHTEXT
ii)ZSO10_FILE
Classes:
i) ZCL_REG_STRING
ii)ZCL_REGEX_STRING
iii)ZCL_INDEX_STRING
# Initial Steps before executing the program
1) Create the texts in ZS010 transaction corresponding to the documents that have been provided.In this case, three documents have been provided for the test. Hence three texts ZTEXT1, ZTEXT2 and ZTEXT3 have been created in SO10 transaction corresponding to the three documents that have been provided.
2) Upload the texts to the table ZSO10_SRCHTEXT.
3) Maintain the entries in the table ZSO10_FILE corresponding to the texts that have been uploaded to the table ZSO10_SRCHTEXT. In this case, three entries have been created : i) ZTEXT1 - French_Armed_Forces.txt, ii)ZTEXT2 - Hitchhikers.txt iii)ZTEXT3 - Warp.txt
4)Ensure that a full text index has been created on table ZSO10_SRCHTEXT
