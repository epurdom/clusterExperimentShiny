# Install R package from github

You can install the github version via

<pre>
library(devtools)
install_github("epurdom/clusterExperimentShiny")
</pre>

# Other required Packages

You must also install `clusterExperiment` from bioconductor,

<pre>
source("https://bioconductor.org/biocLite.R")
biocLite("clusterExperiment")
</pre>