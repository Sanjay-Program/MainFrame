//DEFINERRDS JOB (ACCT),'DEFINE VSAM RRDS',CLASS=A,MSGCLASS=X,
//             NOTIFY=&SYSUID,REGION=4M
//STEP1    EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
       DEFINE CLUSTER -
               (NAME(USER.VSAM.RRDS) -
               RECORDSIZE(80 80) -
               NUMBERED) -
               DATA -
               (NAME(USER.VSAM.RRDS.DATA) -
               CYLINDERS(3 3) -
               SPACE(3 3))
       IF LASTCC = 0 THEN SET MAXCC = 0
/*
