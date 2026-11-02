
-- File Name       : CDUSRR01 (Index)
-- Author          : Subu
-- Date            : 02/11/2024
-- Description     : Users Master - Update Index

CREATE INDEX CDUSRR01
ON CDUSRP (USERID ASC)
NOT PARTITIONED ;

LABEL ON INDEX CDUSRR01
IS 'CardDemo - Key by User ID (SQL)' ;

