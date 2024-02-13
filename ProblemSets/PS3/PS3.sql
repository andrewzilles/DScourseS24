--Read in the data
.mode csv
.import /home/ouecon016/DScourseS24/ProblemSets/PS3/FL_insurance_sample.csv insurance

--Print the 10 rows of data
SELECT * FROM insurance LIMIT 10;

--List of countries in the sample
SELECT DISTINCT county from insurance;

--Calculate the average property appreciation
SELECT AVG(tiv_2012 - tiv_2011) AS average FROM insurance;

--Frequency of wood vs others
SELECT construction, COUNT(*) AS frequncy FROM insurance
GROUP BY construction;
