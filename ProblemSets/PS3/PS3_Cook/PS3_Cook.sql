CREATE TABLE "Insurance"(policyID INTEGER,statecode TEXT,county TEXT,eq_site_limit REAL,hu_site_limitREAL,fl_site_limit REAL,fr_site_limit REAL,tiv_2011 REAL,tiv_2012 REAL,eq_site_deductible REAL,hu_site_deductible REAL,fl_site_deductible REAL,fr_site_deductible REAL,point_latitude REAL,point_longitude REAL,line TEXT,construction TEXT,point_granularity INTEGER);
.mode csv
.import FL_insurance_sample.csv Insurance
DELETE FROM Insurance WHERE county='county';
SELECT * FROM Insurance LIMIT 10;
SELECT county COUNT (*) FROM Insurance GROUP BY county;
SELECT AVG(tiv_2012 - tiv_2011) FROM Insurance;