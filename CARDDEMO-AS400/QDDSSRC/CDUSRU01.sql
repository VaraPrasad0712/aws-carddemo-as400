
-- File Name       : CDUSRU01 (Index)
-- Author          : Subu
-- Date            : 02/11/2024
-- Description     : Users Master - Update Index

CREATE UNIQUE INDEX CDUSRU01
ON CDUSRP (USERID ASC)
NOT PARTITIONED ;

LABEL ON INDEX CDUSRU01
IS 'CardDemo - Update Index (SQL)' ;

