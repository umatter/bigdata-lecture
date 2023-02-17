
# HADOOP EXAMPLE --------------
# Word count

# create directory for input files (typically text files)
mkdir ~/input

# create input text file
echo "Apple Orange Mango
Orange Grapes Plum
Apple Plum Mango
Apple Apple Plum" >>  ~/input/text.txt

# run mapreduce word count
/usr/local/hadoop/bin/hadoop jar /usr/local/hadoop/share/hadoop/mapreduce/hadoop-mapreduce-examples-2.10.1.jar wordcount ~/input ~/wc_example

# inspect output
cat ~/wc_example/*





# SPARK SQL
# (`SPARK-HOME` is again the placeholder for the path to your local Spark installation).
cd SPARK-HOME



## Spark with SQL: JSON example
#- Query the data directly via SQL commands by referring to the location of the JSON file. 
#- Example: *select all observations*

SELECT * 
FROM json.`examples/src/main/resources/employees.json`
;



## Spark with SQL: JSON example
# - Example: *filter observations*

SELECT * 
FROM json.`examples/src/main/resources/employees.json`
WHERE salary <4000



## Spark with SQL: JSON example
# - Example: *compute the average salary*
SELECT AVG(salary) AS mean_salary 
FROM json.`examples/src/main/resources/employees.json`
;



