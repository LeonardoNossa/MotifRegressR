# Advanced genomics project: MotifRegressR


## Background
This project was developed as part of the Advanced genomics course within the Master's degree in 
Bioinformatics for Computational Genomics.

## Authors
This project was developed by [Leonardo Nossa](https://github.com/LeonardoNossa), 
[Federico Camozzi](https://github.com/Federico-Camozzi), [Marco Cominelli](https://github.com/marco-cominelli01), 
[Riccardo de Sury](), [Teodor Devda](https://github.com/doroteo17), [Laura Mottarlini](),
[Gabriele Oliveto](https://github.com/Gab-23) and [Elena Sasso](https://github.com/elenasasso).


## Project Overview
This R package facilitates motif analysis for gene expression studies. It 
integrates upstream DNA sequence information, gene expression profiles, and 
Position Frequency Matrices (PFMs) to identify regulatory motifs influencing 
gene expression.

Users can import PFMs from JASPAR or custom sources, annotate genomic sequences 
with transcription start sites (TSS), and retrieve metadata of gene expression 
conditions.

The package scores motifs across genes, aggregates the results, and performs 
regression analysis to determine the most impactful motifs under different 
experimental conditions. Visualization functions finally help the user in the 
sense-making step of the analyses.


If you want to see some example you can see the [Vignette.html](Vignette.html) file in the [doc](doc) folder.

## How to Use This Repository
If you're interested in running the analysis yourself, please follow the instruction in the [Vignette.html](Vignette.html) 
to install the package.
You can find all the scripts in the [R](R) folder and all the informations about it in the [man](man) folder. 
Please make sure you have R installed.


## Contact
For any additional questions or feedback, please contact [Leonardo Nossa](mailto:leonardo.nossa@studenti.unimi.it).
