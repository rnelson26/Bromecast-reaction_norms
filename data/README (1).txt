10/04/2023

Description of files in google drive BromeCast/genomics

- BRTEcg_SNPs.bed

SNP matrix in bed format where rows are SNP positions in the genome (217206) and 
columns (96) are: 
1) SNP genomic coordinate (format: rs_chromosome_bp), 
2) the reference allele, 
3) the alternate allele, 
4–96) individuals (common garden genotypes; 93 included, 2 are pending sequencing).
The matrix has values 0, 1, or 2 representing the number of reference alleles.
I excluded SNPs that where only present in less than 5% individuals (i.e., maf=0.05).

- BRTEcg_IBSmatrix.txt

93x93 matrix with values ranging from 0 to 1 representing the fraction of identity 
by state (i.e., allele sharing) for each pair of individuals (common garden genotypes)

- BRTEcg_genotypesCode.csv

Refer to this for matching sample number assigned in the genomic files (PopNum column) 
with sample number assigned in the common gardens (genotype column).

genotype and source are the names used in the common gardens
NewSiteCode and PopNum are the names used in the genomic files and csv files below 

- BRTE307regions.csv
Geographic information of all samples in the genomic dataset including coordinates, 
range (invaded or native), country, and region (in north America it follows the epa level II ecoregions: https://www.epa.gov/eco-research/ecoregions-north-america)

- BRTE307climate.csv
Environmental variables extracted from coordinates of origin from the CHELSA database.
For specifications see: chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/https://chelsa-climate.org/wp-admin/download-page/CHELSA_tech_specification_V2.pdf
Not including elevation, which can be obtained in R with library(elevatr) and get_elev_raster() based on coordinates.

Some that could be relevant for modeling:
Annual Mean Temperature, 
Temperature Annual Range, 
Mean Diurnal Range, 
Annual Precipitation, 
Precipitation Seasonality,
Mean monthly Potential Evapotranspiration,
Elevation

- getSNPs.R

The R code I used to produce the SNP and IBS matrices; for reference. 
I have used the package SNPRelate to prepare files for GWAS in gemma. 
It also performs several analyses on genomic data (e.g., PCA, IBS, IBD)

- 307BRTE.imputed_LDfiltered.gds

The genomic dataset (SNPs filtered for linkage disequilibrium) with all samples (307) sequenced so far.
This is the format that SNPRelate uses.
Individuals are stored in the sample.id object which corresponds to PopNum in the csv files.
