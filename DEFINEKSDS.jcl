//DEFINEKSDS JOB (ACCT),'DEFINE VSAM KSDS',CLASS=A,MSGCLASS=X,
//             NOTIFY=&SYSUID,REGION=4M
//STEP1    EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
       DEFINE CLUSTER -
               (NAME(USER.VSAM.KSDS) -
               RECORDSIZE(100 100) -
               KEYS(10 0) -
               INDEXED) -
               DATA -
               (NAME(USER.VSAM.KSDS.DATA) -
               CYLINDERS(5 5) -
               SPACE(5 5)) -
               INDEX -
               (NAME(USER.VSAM.KSDS.INDEX) -
               CYLINDERS(1 1))
       IF LASTCC = 0 THEN -
           SET MAXCC = 0
       ELSE -
           SET MAXCC = 16
/*
