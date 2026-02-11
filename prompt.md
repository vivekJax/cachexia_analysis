I am interested in building an interactive tool to analyze this dataset. 

Present a phased plan for this with clear tests.

The data is mouse study described here @Cachexia Study - Kumar2025.1.pptx 


Two groups of mice were tested in digital cages. The results are here @CCX_C1_animal_1min_2026-01-28T18_21_57Z.csv . 

The cage name field is `cage.name'. 

Each cage has 3 animals. 

The digital cage data consists of phenotypes measured in 1 minute bins. 

Some cages have mixed groups. 

This is stated in the 'group.name'. 

Which animal has received what treatment is in the @Final Cachexia in CD2 F1 mice with CT-26.xlsx described below. 

The mice had manual body condition score and weights collected here @Final Cachexia in CD2 F1 mice with CT-26.xlsx .

I would like a data loader. This should allow me to check the assigment of groups and individual ID of mice for treatment. 

I would like a module for human scored data analysis, such as Body Condition Score (BCS) and BodyWeight (BW).

I would like a module for the analysis of digital cage data. 

I'm interested in modeling if digital cage can detect the onset of Cachexia faster than human scoring. If the digital measures are more sensitive (require less animals) than manual methods. 

I like to work in R, however please make sure that the large data can be efficiently operated on using R. If you find that it is slow, then swich to Python. I'm open to suggestions. 

Please document all code in a manner that someone who is new to R can understand it. 

Justify your choices in the methods.

Please place phased software design file in the working folder as a markdown file. 