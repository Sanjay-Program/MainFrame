//DEFINEESDS JOB (ACCT),'DEFINE VSAM ESDS',CLASS=A,MSGCLASS=X,
//             NOTIFY=&SYSUID,REGION=4M
//STEP1    EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
       DEFINE CLUSTER -
               (NAME(USER.VSAM.ESDS) -
               RECORDSIZE(100 100) -
               NONINDEXED) -
               DATA -
               (NAME(USER.VSAM.ESDS.DATA) -
               CYLINDERS(5 5) -
               SPACE(5 5))
       IF LASTCC = 0 THEN SET MAXCC = 0
/*
