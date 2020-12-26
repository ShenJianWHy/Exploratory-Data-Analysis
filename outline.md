
# eaR

- propTable: output a real table (or html table) to make the output more elegant(may use knitr for this purpose)


# Master Data Tool
- [ ] **Ref**  http://stla.github.io/stlapblog/posts/shiny_editTable.html
https://jrowen.github.io/rhandsontable/#dropdown__autocomplete   
for masterdatatool schema set part


# EADA & Neonet Project

## To do List

- [ ] Add unit(week, month , year) option in monitoring part

- [ ] package test: first build package; then write test which would be a huge benefit in app development

- [ ] recommend: not use pivot table package(very slow), implement basic pivot table your self.

- [ ] integrate all operation with data(from deep dive, monitoring) to only one variable named like "finDat" for efficient memory usage aim, which would be used in whole APP.  
    **Bug:** in data integration server part, finData() is not defined  
    **Note:** modify code in dfsummary part to show any changes in "finDat"

- [ ] ![Global Variables](www/globalVarsRequest.png)   

- [ ] original data  policynumber matching **issues**.


- [ ] code optimization(encapsulated in module)
- [ ] Add Pivot chart for pivot part

- [ ] Full functionality for bin width set (beatiful UI, create new field to save new bin width):      
  - by DT package, adding one bin, editing bin width directly in table   ~ **1 day**  
  - or adding a line which includes customized number of movable ticks, by leveraging plotly pkg if possible. ~ **2 days**  
  - or D3.js ~ **unkown days**

  **Ref**   https://plotly.com/r/sliders/


- [ ] TS: choices : by week/month/year | sum/average | Rolling n periods (months/weeks)

- [ ] add leakage
      ap vs tp
      -->
- [ ] two way anova analysis one way analysis

- [ ] if one var is selected, then in another selectinput, it wont show for bivarate analysis



## Objectives
- **Automatically** generating data analysis report with only few sets, which includes all meaningful insights. With these insights in mind, user could even further explore data.
- Provide an user-friendly, powerful and interactive interface for data integration and visualization.
- With ML techniques, predict and analyse the result to support business decision.
- May provide a smooth project management system
- Neonet:
 - find segments that are profitable
 - selling products but at the correct price points
 - Traditional BI monitoring functionality

## Sections

### Navi Bar

### Home Page
all recorded activities of the user  
all projects available

### Data Integration

### Global Variables

### Deep Dive
keep all the data operation and plotting setting for next use/ or for another user who want to have the same result.


- put the things in correct place
- three analysis layout setting
- bivariate (one setting for all plotting for bivariate)




![kpi box](www/kpibox.png)   
