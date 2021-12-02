#lang racket/base
(require racket/string
         racket/format
         racket/set)
(provide (all-defined-out))

;; This module contains the semi-processed tables used to generate keywords.rktd.

;; table : (Hash String => (Listof Symbol))
(define table (make-hash))

;; table-add! : String (Listof Symbol) -> Void
(define (table-add! word vals)
  (hash-set! table word (set-union (hash-ref table word null) vals)))

;; show-table : [(Listof Symbol)] -> Void
(define (show-table [only-vals #f])
  (define max-word-len
    (apply max (for/list ([w (hash-keys table)]) (string-length w))))
  (define word-space (+ max-word-len 2))
  (define all-vals
    (or only-vals
        (let ([vals-set (for/fold ([acc null]) ([(w vals) (in-hash table)]) (set-union acc vals))])
          (sort vals-set symbol<?))))
  (for ([word (sort (hash-keys table) string<?)])
    (define vals (hash-ref table word))
    (when (for/or ([val vals]) (memq val all-vals))
      (printf "( ~a" (~a word #:width word-space))
      (for ([val all-vals])
        (cond [(memq val vals)
               (printf "~a " val)]
              [else
               (write-string (make-string (add1 (string-length (symbol->string val))) #\space))]))
      (printf " )\n"))))

;; ================================================================================

;; Source: http://www.postgresql.org/docs/current/static/sql-keywords-appendix.html

;; 0         1         2         3         4         5         6
;; 0123456789012345678901234567890123456789012345678901234567890
;; Key Word                        pg    '11   '08   '92

;; RS* = NOT RESERVED, but can't be function or type
;; no* = RESERVED, but can be function or type

(define pg-table-src #<<end-of-table
A                                     n     n        
ABORT                           n                    
ABS                                   RSV   RSV      
ABSENT                                n     n        
ABSOLUTE                        n     n     n     RSV
ACCESS                          n                    
ACCORDING                             n     n        
ACTION                          n     n     n     RSV
ADA                                   n     n     n  
ADD                             n     n     n     RSV
ADMIN                           n     n     n        
AFTER                           n     n     n        
AGGREGATE                       n                    
ALL                             RSV   RSV   RSV   RSV
ALLOCATE                              RSV   RSV   RSV
ALSO                            n                    
ALTER                           n     RSV   RSV   RSV
ALWAYS                          n     n     n        
ANALYSE                         RSV                  
ANALYZE                         RSV                  
AND                             RSV   RSV   RSV   RSV
ANY                             RSV   RSV   RSV   RSV
ARE                                   RSV   RSV   RSV
ARRAY                           RSV   RSV   RSV      
ARRAY_AGG                             RSV   RSV      
ARRAY_MAX_CARDINALITY                 RSV            
AS                              RSV   RSV   RSV   RSV
ASC                             RSV   n     n     RSV
ASENSITIVE                            RSV   RSV      
ASSERTION                       n     n     n     RSV
ASSIGNMENT                      n     n     n        
ASYMMETRIC                      RSV   RSV   RSV      
AT                              n     RSV   RSV   RSV
ATOMIC                                RSV   RSV      
ATTRIBUTE                       n     n     n        
ATTRIBUTES                            n     n        
AUTHORIZATION                   RSV   RSV   RSV   RSV
AVG                                   RSV   RSV   RSV
BACKWARD                        n                    
BASE64                                n     n        
BEFORE                          n     n     n        
BEGIN                           n     RSV   RSV   RSV
BEGIN_FRAME                           RSV            
BEGIN_PARTITION                       RSV            
BERNOULLI                             n     n        
BETWEEN                         RS*   RSV   RSV   RSV
BIGINT                          RS*   RSV   RSV      
BINARY                          no*   RSV   RSV      
BIT                             RS*               RSV
BIT_LENGTH                                        RSV
BLOB                                  RSV   RSV      
BLOCKED                               n     n        
BOM                                   n     n        
BOOLEAN                         RS*   RSV   RSV      
BOTH                            RSV   RSV   RSV   RSV
BREADTH                               n     n        
BY                              n     RSV   RSV   RSV
C                                     n     n     n  
CACHE                           n                    
CALL                                  RSV   RSV      
CALLED                          n     RSV   RSV      
CARDINALITY                           RSV   RSV      
CASCADE                         n     n     n     RSV
CASCADED                        n     RSV   RSV   RSV
CASE                            RSV   RSV   RSV   RSV
CAST                            RSV   RSV   RSV   RSV
CATALOG                         n     n     n     RSV
CATALOG_NAME                          n     n     n  
CEIL                                  RSV   RSV      
CEILING                               RSV   RSV      
CHAIN                           n     n     n        
CHAR                            RS*   RSV   RSV   RSV
CHARACTER                       RS*   RSV   RSV   RSV
CHARACTERISTICS                 n     n     n        
CHARACTERS                            n     n        
CHARACTER_LENGTH                      RSV   RSV   RSV
CHARACTER_SET_CATALOG                 n     n     n  
CHARACTER_SET_NAME                    n     n     n  
CHARACTER_SET_SCHEMA                  n     n     n  
CHAR_LENGTH                           RSV   RSV   RSV
CHECK                           RSV   RSV   RSV   RSV
CHECKPOINT                      n                    
CLASS                           n                    
CLASS_ORIGIN                          n     n     n  
CLOB                                  RSV   RSV      
CLOSE                           n     RSV   RSV   RSV
CLUSTER                         n                    
COALESCE                        RS*   RSV   RSV   RSV
COBOL                                 n     n     n  
COLLATE                         RSV   RSV   RSV   RSV
COLLATION                       no*   n     n     RSV
COLLATION_CATALOG                     n     n     n  
COLLATION_NAME                        n     n     n  
COLLATION_SCHEMA                      n     n     n  
COLLECT                               RSV   RSV      
COLUMN                          RSV   RSV   RSV   RSV
COLUMNS                               n     n        
COLUMN_NAME                           n     n     n  
COMMAND_FUNCTION                      n     n     n  
COMMAND_FUNCTION_CODE                 n     n        
COMMENT                         n                    
COMMENTS                        n                    
COMMIT                          n     RSV   RSV   RSV
COMMITTED                       n     n     n     n  
CONCURRENTLY                    no*                  
CONDITION                             RSV   RSV      
CONDITION_NUMBER                      n     n     n  
CONFIGURATION                   n                    
CONFLICT                        n                    
CONNECT                               RSV   RSV   RSV
CONNECTION                      n     n     n     RSV
CONNECTION_NAME                       n     n     n  
CONSTRAINT                      RSV   RSV   RSV   RSV
CONSTRAINTS                     n     n     n     RSV
CONSTRAINT_CATALOG                    n     n     n  
CONSTRAINT_NAME                       n     n     n  
CONSTRAINT_SCHEMA                     n     n     n  
CONSTRUCTOR                           n     n        
CONTAINS                              RSV   n        
CONTENT                         n     n     n        
CONTINUE                        n     n     n     RSV
CONTROL                               n     n        
CONVERSION                      n                    
CONVERT                               RSV   RSV   RSV
COPY                            n                    
CORR                                  RSV   RSV      
CORRESPONDING                         RSV   RSV   RSV
COST                            n                    
COUNT                                 RSV   RSV   RSV
COVAR_POP                             RSV   RSV      
COVAR_SAMP                            RSV   RSV      
CREATE                          RSV   RSV   RSV   RSV
CROSS                           no*   RSV   RSV   RSV
CSV                             n                    
CUBE                            n     RSV   RSV      
CUME_DIST                             RSV   RSV      
CURRENT                         n     RSV   RSV   RSV
CURRENT_CATALOG                 RSV   RSV   RSV      
CURRENT_DATE                    RSV   RSV   RSV   RSV
CURRENT_PATH                          RSV   RSV      
CURRENT_ROLE                    RSV   RSV   RSV      
CURRENT_ROW                           RSV            
CURRENT_SCHEMA                  no*   RSV   RSV      
CURRENT_TIME                    RSV   RSV   RSV   RSV
CURRENT_TIMESTAMP               RSV   RSV   RSV   RSV
CURRENT_USER                    RSV   RSV   RSV   RSV
CURSOR                          n     RSV   RSV   RSV
CURSOR_NAME                           n     n     n  
CYCLE                           n     RSV   RSV      
DATA                            n     n     n     n  
DATABASE                        n                    
DATALINK                              RSV   RSV      
DATE                                  RSV   RSV   RSV
DATETIME_INTERVAL_CODE                n     n     n  
DATETIME_INTERVAL_PRECISION           n     n     n  
DAY                             n     RSV   RSV   RSV
DB                                    n     n        
DEALLOCATE                      n     RSV   RSV   RSV
DEC                             RS*   RSV   RSV   RSV
DECIMAL                         RS*   RSV   RSV   RSV
DECLARE                         n     RSV   RSV   RSV
DEFAULT                         RSV   RSV   RSV   RSV
DEFAULTS                        n     n     n        
DEFERRABLE                      RSV   n     n     RSV
DEFERRED                        n     n     n     RSV
DEFINED                               n     n        
DEFINER                         n     n     n        
DEGREE                                n     n        
DELETE                          n     RSV   RSV   RSV
DELIMITER                       n                    
DELIMITERS                      n                    
DENSE_RANK                            RSV   RSV      
DEPTH                                 n     n        
DEREF                                 RSV   RSV      
DERIVED                               n     n        
DESC                            RSV   n     n     RSV
DESCRIBE                              RSV   RSV   RSV
DESCRIPTOR                            n     n     RSV
DETERMINISTIC                         RSV   RSV      
DIAGNOSTICS                           n     n     RSV
DICTIONARY                      n                    
DISABLE                         n                    
DISCARD                         n                    
DISCONNECT                            RSV   RSV   RSV
DISPATCH                              n     n        
DISTINCT                        RSV   RSV   RSV   RSV
DLNEWCOPY                             RSV   RSV      
DLPREVIOUSCOPY                        RSV   RSV      
DLURLCOMPLETE                         RSV   RSV      
DLURLCOMPLETEONLY                     RSV   RSV      
DLURLCOMPLETEWRITE                    RSV   RSV      
DLURLPATH                             RSV   RSV      
DLURLPATHONLY                         RSV   RSV      
DLURLPATHWRITE                        RSV   RSV      
DLURLSCHEME                           RSV   RSV      
DLURLSERVER                           RSV   RSV      
DLVALUE                               RSV   RSV      
DO                              RSV                  
DOCUMENT                        n     n     n        
DOMAIN                          n     n     n     RSV
DOUBLE                          n     RSV   RSV   RSV
DROP                            n     RSV   RSV   RSV
DYNAMIC                               RSV   RSV      
DYNAMIC_FUNCTION                      n     n     n  
DYNAMIC_FUNCTION_CODE                 n     n        
EACH                            n     RSV   RSV      
ELEMENT                               RSV   RSV      
ELSE                            RSV   RSV   RSV   RSV
EMPTY                                 n     n        
ENABLE                          n                    
ENCODING                        n     n     n        
ENCRYPTED                       n                    
END                             RSV   RSV   RSV   RSV
END-EXEC                              RSV   RSV   RSV
END_FRAME                             RSV            
END_PARTITION                         RSV            
ENFORCED                              n              
ENUM                            n                    
EQUALS                                RSV   n        
ESCAPE                          n     RSV   RSV   RSV
EVENT                           n                    
EVERY                                 RSV   RSV      
EXCEPT                          RSV   RSV   RSV   RSV
EXCEPTION                                         RSV
EXCLUDE                         n     n     n        
EXCLUDING                       n     n     n        
EXCLUSIVE                       n                    
EXEC                                  RSV   RSV   RSV
EXECUTE                         n     RSV   RSV   RSV
EXISTS                          RS*   RSV   RSV   RSV
EXP                                   RSV   RSV      
EXPLAIN                         n                    
EXPRESSION                            n              
EXTENSION                       n                    
EXTERNAL                        n     RSV   RSV   RSV
EXTRACT                         RS*   RSV   RSV   RSV
FALSE                           RSV   RSV   RSV   RSV
FAMILY                          n                    
FETCH                           RSV   RSV   RSV   RSV
FILE                                  n     n        
FILTER                          n     RSV   RSV      
FINAL                                 n     n        
FIRST                           n     n     n     RSV
FIRST_VALUE                           RSV   RSV      
FLAG                                  n     n        
FLOAT                           RS*   RSV   RSV   RSV
FLOOR                                 RSV   RSV      
FOLLOWING                       n     n     n        
FOR                             RSV   RSV   RSV   RSV
FORCE                           n                    
FOREIGN                         RSV   RSV   RSV   RSV
FORTRAN                               n     n     n  
FORWARD                         n                    
FOUND                                 n     n     RSV
FRAME_ROW                             RSV            
FREE                                  RSV   RSV      
FREEZE                          no*                  
FROM                            RSV   RSV   RSV   RSV
FS                                    n     n        
FULL                            no*   RSV   RSV   RSV
FUNCTION                        n     RSV   RSV      
FUNCTIONS                       n                    
FUSION                                RSV   RSV      
G                                     n     n        
GENERAL                               n     n        
GENERATED                             n     n        
GET                                   RSV   RSV   RSV
GLOBAL                          n     RSV   RSV   RSV
GO                                    n     n     RSV
GOTO                                  n     n     RSV
GRANT                           RSV   RSV   RSV   RSV
GRANTED                         n     n     n        
GREATEST                        RS*                  
GROUP                           RSV   RSV   RSV   RSV
GROUPING                        RS*   RSV   RSV      
GROUPS                                RSV            
HANDLER                         n                    
HAVING                          RSV   RSV   RSV   RSV
HEADER                          n                    
HEX                                   n     n        
HIERARCHY                             n     n        
HOLD                            n     RSV   RSV      
HOUR                            n     RSV   RSV   RSV
ID                                    n     n        
IDENTITY                        n     RSV   RSV   RSV
IF                              n                    
IGNORE                                n     n        
ILIKE                           no*                  
IMMEDIATE                       n     n     n     RSV
IMMEDIATELY                           n              
IMMUTABLE                       n                    
IMPLEMENTATION                        n     n        
IMPLICIT                        n                    
IMPORT                          n     RSV   RSV      
IN                              RSV   RSV   RSV   RSV
INCLUDING                       n     n     n        
INCREMENT                       n     n     n        
INDENT                                n     n        
INDEX                           n                    
INDEXES                         n                    
INDICATOR                             RSV   RSV   RSV
INHERIT                         n                    
INHERITS                        n                    
INITIALLY                       RSV   n     n     RSV
INLINE                          n                    
INNER                           no*   RSV   RSV   RSV
INOUT                           RS*   RSV   RSV      
INPUT                           n     n     n     RSV
INSENSITIVE                     n     RSV   RSV   RSV
INSERT                          n     RSV   RSV   RSV
INSTANCE                              n     n        
INSTANTIABLE                          n     n        
INSTEAD                         n     n     n        
INT                             RS*   RSV   RSV   RSV
INTEGER                         RS*   RSV   RSV   RSV
INTEGRITY                             n     n        
INTERSECT                       RSV   RSV   RSV   RSV
INTERSECTION                          RSV   RSV      
INTERVAL                        RS*   RSV   RSV   RSV
INTO                            RSV   RSV   RSV   RSV
INVOKER                         n     n     n        
IS                              no*   RSV   RSV   RSV
ISNULL                          no*                  
ISOLATION                       n     n     n     RSV
JOIN                            no*   RSV   RSV   RSV
K                                     n     n        
KEY                             n     n     n     RSV
KEY_MEMBER                            n     n        
KEY_TYPE                              n     n        
LABEL                           n                    
LAG                                   RSV   RSV      
LANGUAGE                        n     RSV   RSV   RSV
LARGE                           n     RSV   RSV      
LAST                            n     n     n     RSV
LAST_VALUE                            RSV   RSV      
LATERAL                         RSV   RSV   RSV      
LEAD                                  RSV   RSV      
LEADING                         RSV   RSV   RSV   RSV
LEAKPROOF                       n                    
LEAST                           RS*                  
LEFT                            no*   RSV   RSV   RSV
LENGTH                                n     n     n  
LEVEL                           n     n     n     RSV
LIBRARY                               n     n        
LIKE                            no*   RSV   RSV   RSV
LIKE_REGEX                            RSV   RSV      
LIMIT                           RSV   n     n        
LINK                                  n     n        
LISTEN                          n                    
LN                                    RSV   RSV      
LOAD                            n                    
LOCAL                           n     RSV   RSV   RSV
LOCALTIME                       RSV   RSV   RSV      
LOCALTIMESTAMP                  RSV   RSV   RSV      
LOCATION                        n     n     n        
LOCATOR                               n     n        
LOCK                            n                    
LOCKED                          n                    
LOGGED                          n                    
LOWER                                 RSV   RSV   RSV
M                                     n     n        
MAP                                   n     n        
MAPPING                         n     n     n        
MATCH                           n     RSV   RSV   RSV
MATCHED                               n     n        
MATERIALIZED                    n                    
MAX                                   RSV   RSV   RSV
MAXVALUE                        n     n     n        
MAX_CARDINALITY                             RSV      
MEMBER                                RSV   RSV      
MERGE                                 RSV   RSV      
MESSAGE_LENGTH                        n     n     n  
MESSAGE_OCTET_LENGTH                  n     n     n  
MESSAGE_TEXT                          n     n     n  
METHOD                                RSV   RSV      
MIN                                   RSV   RSV   RSV
MINUTE                          n     RSV   RSV   RSV
MINVALUE                        n     n     n        
MOD                                   RSV   RSV      
MODE                            n                    
MODIFIES                              RSV   RSV      
MODULE                                RSV   RSV   RSV
MONTH                           n     RSV   RSV   RSV
MORE                                  n     n     n  
MOVE                            n                    
MULTISET                              RSV   RSV      
MUMPS                                 n     n     n  
NAME                            n     n     n     n  
NAMES                           n     n     n     RSV
NAMESPACE                             n     n        
NATIONAL                        RS*   RSV   RSV   RSV
NATURAL                         no*   RSV   RSV   RSV
NCHAR                           RS*   RSV   RSV   RSV
NCLOB                                 RSV   RSV      
NESTING                               n     n        
NEW                                   RSV   RSV      
NEXT                            n     n     n     RSV
NFC                                   n     n        
NFD                                   n     n        
NFKC                                  n     n        
NFKD                                  n     n        
NIL                                   n     n        
NO                              n     RSV   RSV   RSV
NONE                            RS*   RSV   RSV      
NORMALIZE                             RSV   RSV      
NORMALIZED                            n     n        
NOT                             RSV   RSV   RSV   RSV
NOTHING                         n                    
NOTIFY                          n                    
NOTNULL                         no*                  
NOWAIT                          n                    
NTH_VALUE                             RSV   RSV      
NTILE                                 RSV   RSV      
NULL                            RSV   RSV   RSV   RSV
NULLABLE                              n     n     n  
NULLIF                          RS*   RSV   RSV   RSV
NULLS                           n     n     n        
NUMBER                                n     n     n  
NUMERIC                         RS*   RSV   RSV   RSV
OBJECT                          n     n     n        
OCCURRENCES_REGEX                     RSV   RSV      
OCTETS                                n     n        
OCTET_LENGTH                          RSV   RSV   RSV
OF                              n     RSV   RSV   RSV
OFF                             n     n     n        
OFFSET                          RSV   RSV   RSV      
OIDS                            n                    
OLD                                   RSV   RSV      
ON                              RSV   RSV   RSV   RSV
ONLY                            RSV   RSV   RSV   RSV
OPEN                                  RSV   RSV   RSV
OPERATOR                        n                    
OPTION                          n     n     n     RSV
OPTIONS                         n     n     n        
OR                              RSV   RSV   RSV   RSV
ORDER                           RSV   RSV   RSV   RSV
ORDERING                              n     n        
ORDINALITY                      n     n     n        
OTHERS                                n     n        
OUT                             RS*   RSV   RSV      
OUTER                           no*   RSV   RSV   RSV
OUTPUT                                n     n     RSV
OVER                            n     RSV   RSV      
OVERLAPS                        no*   RSV   RSV   RSV
OVERLAY                         RS*   RSV   RSV      
OVERRIDING                            n     n        
OWNED                           n                    
OWNER                           n                    
P                                     n     n        
PAD                                   n     n     RSV
PARAMETER                             RSV   RSV      
PARAMETER_MODE                        n     n        
PARAMETER_NAME                        n     n        
PARAMETER_ORDINAL_POSITION            n     n        
PARAMETER_SPECIFIC_CATALOG            n     n        
PARAMETER_SPECIFIC_NAME               n     n        
PARAMETER_SPECIFIC_SCHEMA             n     n        
PARSER                          n                    
PARTIAL                         n     n     n     RSV
PARTITION                       n     RSV   RSV      
PASCAL                                n     n     n  
PASSING                         n     n     n        
PASSTHROUGH                           n     n        
PASSWORD                        n                    
PATH                                  n     n        
PERCENT                               RSV            
PERCENTILE_CONT                       RSV   RSV      
PERCENTILE_DISC                       RSV   RSV      
PERCENT_RANK                          RSV   RSV      
PERIOD                                RSV            
PERMISSION                            n     n        
PLACING                         RSV   n     n        
PLANS                           n                    
PLI                                   n     n     n  
POLICY                          n                    
PORTION                               RSV            
POSITION                        RS*   RSV   RSV   RSV
POSITION_REGEX                        RSV   RSV      
POWER                                 RSV   RSV      
PRECEDES                              RSV            
PRECEDING                       n     n     n        
PRECISION                       RS*   RSV   RSV   RSV
PREPARE                         n     RSV   RSV   RSV
PREPARED                        n                    
PRESERVE                        n     n     n     RSV
PRIMARY                         RSV   RSV   RSV   RSV
PRIOR                           n     n     n     RSV
PRIVILEGES                      n     n     n     RSV
PROCEDURAL                      n                    
PROCEDURE                       n     RSV   RSV   RSV
PROGRAM                         n                    
PUBLIC                                n     n     RSV
QUOTE                           n                    
RANGE                           n     RSV   RSV      
RANK                                  RSV   RSV      
READ                            n     n     n     RSV
READS                                 RSV   RSV      
REAL                            RS*   RSV   RSV   RSV
REASSIGN                        n                    
RECHECK                         n                    
RECOVERY                              n     n        
RECURSIVE                       n     RSV   RSV      
REF                             n     RSV   RSV      
REFERENCES                      RSV   RSV   RSV   RSV
REFERENCING                           RSV   RSV      
REFRESH                         n                    
REGR_AVGX                             RSV   RSV      
REGR_AVGY                             RSV   RSV      
REGR_COUNT                            RSV   RSV      
REGR_INTERCEPT                        RSV   RSV      
REGR_R2                               RSV   RSV      
REGR_SLOPE                            RSV   RSV      
REGR_SXX                              RSV   RSV      
REGR_SXY                              RSV   RSV      
REGR_SYY                              RSV   RSV      
REINDEX                         n                    
RELATIVE                        n     n     n     RSV
RELEASE                         n     RSV   RSV      
RENAME                          n                    
REPEATABLE                      n     n     n     n  
REPLACE                         n                    
REPLICA                         n                    
REQUIRING                             n     n        
RESET                           n                    
RESPECT                               n     n        
RESTART                         n     n     n        
RESTORE                               n     n        
RESTRICT                        n     n     n     RSV
RESULT                                RSV   RSV      
RETURN                                RSV   RSV      
RETURNED_CARDINALITY                  n     n        
RETURNED_LENGTH                       n     n     n  
RETURNED_OCTET_LENGTH                 n     n     n  
RETURNED_SQLSTATE                     n     n     n  
RETURNING                       RSV   n     n        
RETURNS                         n     RSV   RSV      
REVOKE                          n     RSV   RSV   RSV
RIGHT                           no*   RSV   RSV   RSV
ROLE                            n     n     n        
ROLLBACK                        n     RSV   RSV   RSV
ROLLUP                          n     RSV   RSV      
ROUTINE                               n     n        
ROUTINE_CATALOG                       n     n        
ROUTINE_NAME                          n     n        
ROUTINE_SCHEMA                        n     n        
ROW                             RS*   RSV   RSV      
ROWS                            n     RSV   RSV   RSV
ROW_COUNT                             n     n     n  
ROW_NUMBER                            RSV   RSV      
RULE                            n                    
SAVEPOINT                       n     RSV   RSV      
SCALE                                 n     n     n  
SCHEMA                          n     n     n     RSV
SCHEMA_NAME                           n     n     n  
SCOPE                                 RSV   RSV      
SCOPE_CATALOG                         n     n        
SCOPE_NAME                            n     n        
SCOPE_SCHEMA                          n     n        
SCROLL                          n     RSV   RSV   RSV
SEARCH                          n     RSV   RSV      
SECOND                          n     RSV   RSV   RSV
SECTION                               n     n     RSV
SECURITY                        n     n     n        
SELECT                          RSV   RSV   RSV   RSV
SELECTIVE                             n     n        
SELF                                  n     n        
SENSITIVE                             RSV   RSV      
SEQUENCE                        n     n     n        
SEQUENCES                       n                    
SERIALIZABLE                    n     n     n     n  
SERVER                          n     n     n        
SERVER_NAME                           n     n     n  
SESSION                         n     n     n     RSV
SESSION_USER                    RSV   RSV   RSV   RSV
SET                             n     RSV   RSV   RSV
SETOF                           RS*                  
SETS                            n     n     n        
SHARE                           n                    
SHOW                            n                    
SIMILAR                         no*   RSV   RSV      
SIMPLE                          n     n     n        
SIZE                                  n     n     RSV
SKIP                            n                    
SMALLINT                        RS*   RSV   RSV   RSV
SNAPSHOT                        n                    
SOME                            RSV   RSV   RSV   RSV
SOURCE                                n     n        
SPACE                                 n     n     RSV
SPECIFIC                              RSV   RSV      
SPECIFICTYPE                          RSV   RSV      
SPECIFIC_NAME                         n     n        
SQL                             n     RSV   RSV   RSV
SQLCODE                                           RSV
SQLERROR                                          RSV
SQLEXCEPTION                          RSV   RSV      
SQLSTATE                              RSV   RSV   RSV
SQLWARNING                            RSV   RSV      
SQRT                                  RSV   RSV      
STABLE                          n                    
STANDALONE                      n     n     n        
START                           n     RSV   RSV      
STATE                                 n     n        
STATEMENT                       n     n     n        
STATIC                                RSV   RSV      
STATISTICS                      n                    
STDDEV_POP                            RSV   RSV      
STDDEV_SAMP                           RSV   RSV      
STDIN                           n                    
STDOUT                          n                    
STORAGE                         n                    
STRICT                          n                    
STRIP                           n     n     n        
STRUCTURE                             n     n        
STYLE                                 n     n        
SUBCLASS_ORIGIN                       n     n     n  
SUBMULTISET                           RSV   RSV      
SUBSTRING                       RS*   RSV   RSV   RSV
SUBSTRING_REGEX                       RSV   RSV      
SUCCEEDS                              RSV            
SUM                                   RSV   RSV   RSV
SYMMETRIC                       RSV   RSV   RSV      
SYSID                           n                    
SYSTEM                          n     RSV   RSV      
SYSTEM_TIME                           RSV            
SYSTEM_USER                           RSV   RSV   RSV
T                                     n     n        
TABLE                           RSV   RSV   RSV   RSV
TABLES                          n                    
TABLESAMPLE                     no*   RSV   RSV      
TABLESPACE                      n                    
TABLE_NAME                            n     n     n  
TEMP                            n                    
TEMPLATE                        n                    
TEMPORARY                       n     n     n     RSV
TEXT                            n                    
THEN                            RSV   RSV   RSV   RSV
TIES                                  n     n        
TIME                            RS*   RSV   RSV   RSV
TIMESTAMP                       RS*   RSV   RSV   RSV
TIMEZONE_HOUR                         RSV   RSV   RSV
TIMEZONE_MINUTE                       RSV   RSV   RSV
TO                              RSV   RSV   RSV   RSV
TOKEN                                 n     n        
TOP_LEVEL_COUNT                       n     n        
TRAILING                        RSV   RSV   RSV   RSV
TRANSACTION                     n     n     n     RSV
TRANSACTIONS_COMMITTED                n     n        
TRANSACTIONS_ROLLED_BACK              n     n        
TRANSACTION_ACTIVE                    n     n        
TRANSFORM                       n     n     n        
TRANSFORMS                            n     n        
TRANSLATE                             RSV   RSV   RSV
TRANSLATE_REGEX                       RSV   RSV      
TRANSLATION                           RSV   RSV   RSV
TREAT                           RS*   RSV   RSV      
TRIGGER                         n     RSV   RSV      
TRIGGER_CATALOG                       n     n        
TRIGGER_NAME                          n     n        
TRIGGER_SCHEMA                        n     n        
TRIM                            RS*   RSV   RSV   RSV
TRIM_ARRAY                            RSV   RSV      
TRUE                            RSV   RSV   RSV   RSV
TRUNCATE                        n     RSV   RSV      
TRUSTED                         n                    
TYPE                            n     n     n     n  
TYPES                           n                    
UESCAPE                               RSV   RSV      
UNBOUNDED                       n     n     n        
UNCOMMITTED                     n     n     n     n  
UNDER                                 n     n        
UNENCRYPTED                     n                    
UNION                           RSV   RSV   RSV   RSV
UNIQUE                          RSV   RSV   RSV   RSV
UNKNOWN                         n     RSV   RSV   RSV
UNLINK                                n     n        
UNLISTEN                        n                    
UNLOGGED                        n                    
UNNAMED                               n     n     n  
UNNEST                                RSV   RSV      
UNTIL                           n                    
UNTYPED                               n     n        
UPDATE                          n     RSV   RSV   RSV
UPPER                                 RSV   RSV   RSV
URI                                   n     n        
USAGE                                 n     n     RSV
USER                            RSV   RSV   RSV   RSV
USER_DEFINED_TYPE_CATALOG             n     n        
USER_DEFINED_TYPE_CODE                n     n        
USER_DEFINED_TYPE_NAME                n     n        
USER_DEFINED_TYPE_SCHEMA              n     n        
USING                           RSV   RSV   RSV   RSV
VACUUM                          n                    
VALID                           n     n     n        
VALIDATE                        n                    
VALIDATOR                       n                    
VALUE                           n     RSV   RSV   RSV
VALUES                          RS*   RSV   RSV   RSV
VALUE_OF                              RSV            
VARBINARY                             RSV   RSV      
VARCHAR                         RS*   RSV   RSV   RSV
VARIADIC                        RSV                  
VARYING                         n     RSV   RSV   RSV
VAR_POP                               RSV   RSV      
VAR_SAMP                              RSV   RSV      
VERBOSE                         no*                  
VERSION                         n     n     n        
VERSIONING                            RSV            
VIEW                            n     n     n     RSV
VIEWS                           n                    
VOLATILE                        n                    
WHEN                            RSV   RSV   RSV   RSV
WHENEVER                              RSV   RSV   RSV
WHERE                           RSV   RSV   RSV   RSV
WHITESPACE                      n     n     n        
WIDTH_BUCKET                          RSV   RSV      
WINDOW                          RSV   RSV   RSV      
WITH                            RSV   RSV   RSV   RSV
WITHIN                          n     RSV   RSV      
WITHOUT                         n     RSV   RSV      
WORK                            n     n     n     RSV
WRAPPER                         n     n     n        
WRITE                           n     n     n     RSV
XML                             n     RSV   RSV      
XMLAGG                                RSV   RSV      
XMLATTRIBUTES                   RS*   RSV   RSV      
XMLBINARY                             RSV   RSV      
XMLCAST                               RSV   RSV      
XMLCOMMENT                            RSV   RSV      
XMLCONCAT                       RS*   RSV   RSV      
XMLDECLARATION                        n     n        
XMLDOCUMENT                           RSV   RSV      
XMLELEMENT                      RS*   RSV   RSV      
XMLEXISTS                       RS*   RSV   RSV      
XMLFOREST                       RS*   RSV   RSV      
XMLITERATE                            RSV   RSV      
XMLNAMESPACES                         RSV   RSV      
XMLPARSE                        RS*   RSV   RSV      
XMLPI                           RS*   RSV   RSV      
XMLQUERY                              RSV   RSV      
XMLROOT                         RS*                  
XMLSCHEMA                             n     n        
XMLSERIALIZE                    RS*   RSV   RSV      
XMLTABLE                              RSV   RSV      
XMLTEXT                               RSV   RSV      
XMLVALIDATE                           RSV   RSV      
YEAR                            n     RSV   RSV   RSV
YES                             n     n     n        
ZONE                            n     n     n     RSV
end-of-table
)

(define (R s w) (and (member s '("RSV" "RS*" "no*")) w))

(for ([line (in-lines (open-input-string pg-table-src))])
  (define word  (string-downcase (string-trim (substring line 0 32))))
  (define pgsql (R (string-trim (substring line 32 38)) 'pgsql))
  (define sql11 (R (string-trim (substring line 38 44)) 'sql11))
  (define sql08 (R (string-trim (substring line 44 50)) 'sql08))
  (define sql92 (R (string-trim (substring line 50))    'sql92))
  (table-add! word (filter values (list sql92 pgsql sql11 sql08))))

;; ================================================================================

;; Source: https://dev.mysql.com/doc/refman/5.7/en/keywords.html

(define mysql-table-src #<<end-of-table
ACCESSIBLE **RESERVED**
ACCOUNT
ACTION
ADD **RESERVED**
AFTER
AGAINST
AGGREGATE
ALGORITHM
ALL **RESERVED**
ALTER **RESERVED**
ALWAYS
ANALYSE
ANALYZE **RESERVED**
AND **RESERVED**
ANY
AS **RESERVED**
ASC **RESERVED**
ASCII
ASENSITIVE **RESERVED**
AT
AUTOEXTEND_SIZE
AUTO_INCREMENT
AVG
AVG_ROW_LENGTH
BACKUP
BEFORE **RESERVED**
BEGIN
BETWEEN **RESERVED**
BIGINT **RESERVED**
BINARY **RESERVED**
BINLOG
BIT
BLOB **RESERVED**
BLOCK
BOOL
BOOLEAN
BOTH **RESERVED**
BTREE
BY **RESERVED**
BYTE
CACHE
CALL **RESERVED**
CASCADE **RESERVED**
CASCADED
CASE **RESERVED**
CATALOG_NAME
CHAIN
CHANGE **RESERVED**
CHANGED
CHANNEL
CHAR **RESERVED**
CHARACTER **RESERVED**
CHARSET
CHECK **RESERVED**
CHECKSUM
CIPHER
CLASS_ORIGIN
CLIENT
CLOSE
COALESCE
CODE
COLLATE **RESERVED**
COLLATION
COLUMN **RESERVED**
COLUMNS
COLUMN_FORMAT
COLUMN_NAME
COMMENT
COMMIT
COMMITTED
COMPACT
COMPLETION
COMPRESSED
COMPRESSION
CONCURRENT
CONDITION **RESERVED**
CONNECTION
CONSISTENT
CONSTRAINT **RESERVED**
CONSTRAINT_CATALOG
CONSTRAINT_NAME
CONSTRAINT_SCHEMA
CONTAINS
CONTEXT
CONTINUE **RESERVED**
CONVERT **RESERVED**
CPU
CREATE **RESERVED**
CROSS **RESERVED**
CUBE
CURRENT
CURRENT_DATE **RESERVED**
CURRENT_TIME **RESERVED**
CURRENT_TIMESTAMP **RESERVED**
CURRENT_USER **RESERVED**
CURSOR **RESERVED**
CURSOR_NAME
DATA
DATABASE **RESERVED**
DATABASES **RESERVED**
DATAFILE
DATE
DATETIME
DAY
DAY_HOUR **RESERVED**
DAY_MICROSECOND **RESERVED**
DAY_MINUTE **RESERVED**
DAY_SECOND **RESERVED**
DEALLOCATE
DEC **RESERVED**
DECIMAL **RESERVED**
DECLARE **RESERVED**
DEFAULT **RESERVED**
DEFAULT_AUTH
DEFINER
DELAYED **RESERVED**
DELAY_KEY_WRITE
DELETE **RESERVED**
DESC **RESERVED**
DESCRIBE **RESERVED**
DES_KEY_FILE
DETERMINISTIC **RESERVED**
DIAGNOSTICS
DIRECTORY
DISABLE
DISCARD
DISK
DISTINCT **RESERVED**
DISTINCTROW **RESERVED**
DIV **RESERVED**
DO
DOUBLE **RESERVED**
DROP **RESERVED**
DUAL **RESERVED**
DUMPFILE
DUPLICATE
DYNAMIC
EACH **RESERVED**
ELSE **RESERVED**
ELSEIF **RESERVED**
ENABLE
ENCLOSED **RESERVED**
ENCRYPTION
END
ENDS
ENGINE
ENGINES
ENUM
ERROR
ERRORS
ESCAPE
ESCAPED **RESERVED**
EVENT
EVENTS
EVERY
EXCHANGE
EXECUTE
EXISTS **RESERVED**
EXIT **RESERVED**
EXPANSION
EXPIRE
EXPLAIN **RESERVED**
EXPORT
EXTENDED
EXTENT_SIZE
FALSE **RESERVED**
FAST
FAULTS
FETCH **RESERVED**
FIELDS
FILE
FILE_BLOCK_SIZE
FILTER
FIRST
FIXED
FLOAT **RESERVED**
FLOAT4 **RESERVED**
FLOAT8 **RESERVED**
FLUSH
FOLLOWS
FOR **RESERVED**
FORCE **RESERVED**
FOREIGN **RESERVED**
FORMAT
FOUND
FROM **RESERVED**
FULL
FULLTEXT **RESERVED**
FUNCTION
GENERAL
GENERATED **RESERVED**
GEOMETRY
GEOMETRYCOLLECTION
GET **RESERVED**
GET_FORMAT
GLOBAL
GRANT **RESERVED**
GRANTS
GROUP **RESERVED**
GROUP_REPLICATION
HANDLER
HASH
HAVING **RESERVED**
HELP
HIGH_PRIORITY **RESERVED**
HOST
HOSTS
HOUR
HOUR_MICROSECOND **RESERVED**
HOUR_MINUTE **RESERVED**
HOUR_SECOND **RESERVED**
IDENTIFIED
IF **RESERVED**
IGNORE **RESERVED**
IGNORE_SERVER_IDS
IMPORT
IN **RESERVED**
INDEX **RESERVED**
INDEXES
INFILE **RESERVED**
INITIAL_SIZE
INNER **RESERVED**
INOUT **RESERVED**
INSENSITIVE **RESERVED**
INSERT **RESERVED**
INSERT_METHOD
INSTALL
INSTANCE
INT **RESERVED**
INT1 **RESERVED**
INT2 **RESERVED**
INT3 **RESERVED**
INT4 **RESERVED**
INT8 **RESERVED**
INTEGER **RESERVED**
INTERVAL **RESERVED**
INTO **RESERVED**
INVOKER
IO
IO_AFTER_GTIDS **RESERVED**
IO_BEFORE_GTIDS **RESERVED**
IO_THREAD
IPC
IS **RESERVED**
ISOLATION
ISSUER
ITERATE **RESERVED**
JOIN **RESERVED**
JSON
KEY **RESERVED**
KEYS **RESERVED**
KEY_BLOCK_SIZE
KILL **RESERVED**
LANGUAGE
LAST
LEADING **RESERVED**
LEAVE **RESERVED**
LEAVES
LEFT **RESERVED**
LESS
LEVEL
LIKE **RESERVED**
LIMIT **RESERVED**
LINEAR **RESERVED**
LINES **RESERVED**
LINESTRING
LIST
LOAD **RESERVED**
LOCAL
LOCALTIME **RESERVED**
LOCALTIMESTAMP **RESERVED**
LOCK **RESERVED**
LOCKS
LOGFILE
LOGS
LONG **RESERVED**
LONGBLOB **RESERVED**
LONGTEXT **RESERVED**
LOOP **RESERVED**
LOW_PRIORITY **RESERVED**
MASTER
MASTER_AUTO_POSITION
MASTER_BIND **RESERVED**
MASTER_CONNECT_RETRY
MASTER_DELAY
MASTER_HEARTBEAT_PERIOD
MASTER_HOST
MASTER_LOG_FILE
MASTER_LOG_POS
MASTER_PASSWORD
MASTER_PORT
MASTER_RETRY_COUNT
MASTER_SERVER_ID
MASTER_SSL
MASTER_SSL_CA
MASTER_SSL_CAPATH
MASTER_SSL_CERT
MASTER_SSL_CIPHER
MASTER_SSL_CRL
MASTER_SSL_CRLPATH
MASTER_SSL_KEY
MASTER_SSL_VERIFY_SERVER_CERT **RESERVED**
MASTER_TLS_VERSION
MASTER_USER
MATCH **RESERVED**
MAXVALUE **RESERVED**
MAX_CONNECTIONS_PER_HOUR
MAX_QUERIES_PER_HOUR
MAX_ROWS
MAX_SIZE
MAX_STATEMENT_TIME
MAX_UPDATES_PER_HOUR
MAX_USER_CONNECTIONS
MEDIUM
MEDIUMBLOB **RESERVED**
MEDIUMINT **RESERVED**
MEDIUMTEXT **RESERVED**
MEMORY
MERGE
MESSAGE_TEXT
MICROSECOND
MIDDLEINT **RESERVED**
MIGRATE
MINUTE
MINUTE_MICROSECOND **RESERVED**
MINUTE_SECOND **RESERVED**
MIN_ROWS
MOD **RESERVED**
MODE
MODIFIES **RESERVED**
MODIFY
MONTH
MULTILINESTRING
MULTIPOINT
MULTIPOLYGON
MUTEX
MYSQL_ERRNO
NAME
NAMES
NATIONAL
NATURAL **RESERVED**
NCHAR
NDB
NDBCLUSTER
NEVER
NEW
NEXT
NO
NODEGROUP
NONBLOCKING
NONE
NOT **RESERVED**
NO_WAIT
NO_WRITE_TO_BINLOG **RESERVED**
NULL **RESERVED**
NUMBER
NUMERIC **RESERVED**
NVARCHAR
OFFSET
OLD_PASSWORD
ON **RESERVED**
ONE
ONLY
OPEN
OPTIMIZE **RESERVED**
OPTIMIZER_COSTS **RESERVED**
OPTION **RESERVED**
OPTIONALLY **RESERVED**
OPTIONS
OR **RESERVED**
ORDER **RESERVED**
OUT **RESERVED**
OUTER **RESERVED**
OUTFILE **RESERVED**
OWNER
PACK_KEYS
PAGE
PARSER
PARSE_GCOL_EXPR
PARTIAL
PARTITION **RESERVED**
PARTITIONING
PARTITIONS
PASSWORD
PHASE
PLUGIN
PLUGINS
PLUGIN_DIR
POINT
POLYGON
PORT
PRECEDES
PRECISION **RESERVED**
PREPARE
PRESERVE
PREV
PRIMARY **RESERVED**
PRIVILEGES
PROCEDURE **RESERVED**
PROCESSLIST
PROFILE
PROFILES
PROXY
PURGE **RESERVED**
QUARTER
QUERY
QUICK
RANGE **RESERVED**
READ **RESERVED**
READS **RESERVED**
READ_ONLY
READ_WRITE **RESERVED**
REAL **RESERVED**
REBUILD
RECOVER
REDOFILE
REDO_BUFFER_SIZE
REDUNDANT
REFERENCES **RESERVED**
REGEXP **RESERVED**
RELAY
RELAYLOG
RELAY_LOG_FILE
RELAY_LOG_POS
RELAY_THREAD
RELEASE **RESERVED**
RELOAD
REMOVE
RENAME **RESERVED**
REORGANIZE
REPAIR
REPEAT **RESERVED**
REPEATABLE
REPLACE **RESERVED**
REPLICATE_DO_DB
REPLICATE_DO_TABLE
REPLICATE_IGNORE_DB
REPLICATE_IGNORE_TABLE
REPLICATE_REWRITE_DB
REPLICATE_WILD_DO_TABLE
REPLICATE_WILD_IGNORE_TABLE
REPLICATION
REQUIRE **RESERVED**
RESET
RESIGNAL **RESERVED**
RESTORE
RESTRICT **RESERVED**
RESUME
RETURN **RESERVED**
RETURNED_SQLSTATE
RETURNS
REVERSE
REVOKE **RESERVED**
RIGHT **RESERVED**
RLIKE **RESERVED**
ROLLBACK
ROLLUP
ROTATE
ROUTINE
ROW
ROWS
ROW_COUNT
ROW_FORMAT
RTREE
SAVEPOINT
SCHEDULE
SCHEMA **RESERVED**
SCHEMAS **RESERVED**
SCHEMA_NAME
SECOND
SECOND_MICROSECOND **RESERVED**
SECURITY
SELECT **RESERVED**
SENSITIVE **RESERVED**
SEPARATOR **RESERVED**
SERIAL
SERIALIZABLE
SERVER
SESSION
SET **RESERVED**
SHARE
SHOW **RESERVED**
SHUTDOWN
SIGNAL **RESERVED**
SIGNED
SIMPLE
SLAVE
SLOW
SMALLINT **RESERVED**
SNAPSHOT
SOCKET
SOME
SONAME
SOUNDS
SOURCE
SPATIAL **RESERVED**
SPECIFIC **RESERVED**
SQL **RESERVED**
SQLEXCEPTION **RESERVED**
SQLSTATE **RESERVED**
SQLWARNING **RESERVED**
SQL_AFTER_GTIDS
SQL_AFTER_MTS_GAPS
SQL_BEFORE_GTIDS
SQL_BIG_RESULT **RESERVED**
SQL_BUFFER_RESULT
SQL_CACHE
SQL_CALC_FOUND_ROWS **RESERVED**
SQL_NO_CACHE
SQL_SMALL_RESULT **RESERVED**
SQL_THREAD
SQL_TSI_DAY
SQL_TSI_HOUR
SQL_TSI_MINUTE
SQL_TSI_MONTH
SQL_TSI_QUARTER
SQL_TSI_SECOND
SQL_TSI_WEEK
SQL_TSI_YEAR
SSL **RESERVED**
STACKED
START
STARTING **RESERVED**
STARTS
STATS_AUTO_RECALC
STATS_PERSISTENT
STATS_SAMPLE_PAGES
STATUS
STOP
STORAGE
STORED **RESERVED**
STRAIGHT_JOIN **RESERVED**
STRING
SUBCLASS_ORIGIN
SUBJECT
SUBPARTITION
SUBPARTITIONS
SUPER
SUSPEND
SWAPS
SWITCHES
TABLE **RESERVED**
TABLES
TABLESPACE
TABLE_CHECKSUM
TABLE_NAME
TEMPORARY
TEMPTABLE
TERMINATED **RESERVED**
TEXT
THAN
THEN **RESERVED**
TIME
TIMESTAMP
TIMESTAMPADD
TIMESTAMPDIFF
TINYBLOB **RESERVED**
TINYINT **RESERVED**
TINYTEXT **RESERVED**
TO **RESERVED**
TRAILING **RESERVED**
TRANSACTION
TRIGGER **RESERVED**
TRIGGERS
TRUE **RESERVED**
TRUNCATE
TYPE
TYPES
UNCOMMITTED
UNDEFINED
UNDO **RESERVED**
UNDOFILE
UNDO_BUFFER_SIZE
UNICODE
UNINSTALL
UNION **RESERVED**
UNIQUE **RESERVED**
UNKNOWN
UNLOCK **RESERVED**
UNSIGNED **RESERVED**
UNTIL
UPDATE **RESERVED**
UPGRADE
USAGE **RESERVED**
USE **RESERVED**
USER
USER_RESOURCES
USE_FRM
USING **RESERVED**
UTC_DATE **RESERVED**
UTC_TIME **RESERVED**
UTC_TIMESTAMP **RESERVED**
VALIDATION
VALUE
VALUES **RESERVED**
VARBINARY **RESERVED**
VARCHAR **RESERVED**
VARCHARACTER **RESERVED**
VARIABLES
VARYING **RESERVED**
VIEW
VIRTUAL **RESERVED**
WAIT
WARNINGS
WEEK
WEIGHT_STRING
WHEN **RESERVED**
WHERE **RESERVED**
WHILE **RESERVED**
WITH **RESERVED**
WITHOUT
WORK
WRAPPER
WRITE **RESERVED**
X509
XA
XID
XML
XOR **RESERVED**
YEAR
YEAR_MONTH **RESERVED**
ZEROFILL **RESERVED**
end-of-table
)

(for ([line (in-lines (open-input-string mysql-table-src))])
  (define linein (open-input-string line))
  (define word (string-downcase (symbol->string (read linein))))
  (define next (read linein))
  (table-add! word (if (eq? next '**RESERVED**) '(mysql) '())))

;; ================================================================================

;; Source: https://sqlite.org/lang_keywords.html

(define sqlite-table-src
  (map string-downcase
       (map symbol->string
            '(ABORT ACTION ADD AFTER ALL ALTER ANALYZE AND
              AS ASC ATTACH AUTOINCREMENT BEFORE BEGIN BETWEEN BY CASCADE CASE CAST
              CHECK COLLATE COLUMN COMMIT CONFLICT CONSTRAINT CREATE CROSS
              CURRENT_DATE CURRENT_TIME CURRENT_TIMESTAMP DATABASE DEFAULT
              DEFERRABLE DEFERRED DELETE DESC DETACH DISTINCT DROP EACH ELSE END
              ESCAPE EXCEPT EXCLUSIVE EXISTS EXPLAIN FAIL FOR FOREIGN FROM FULL GLOB
              GROUP HAVING IF IGNORE IMMEDIATE IN INDEX INDEXED INITIALLY INNER
              INSERT INSTEAD INTERSECT INTO IS ISNULL JOIN KEY LEFT LIKE LIMIT MATCH
              NATURAL NO NOT NOTNULL NULL OF OFFSET ON OR ORDER OUTER PLAN PRAGMA
              PRIMARY QUERY RAISE RECURSIVE REFERENCES REGEXP REINDEX RELEASE RENAME
              REPLACE RESTRICT RIGHT ROLLBACK ROW SAVEPOINT SELECT SET TABLE TEMP
              TEMPORARY THEN TO TRANSACTION TRIGGER UNION UNIQUE UPDATE USING VACUUM
              VALUES VIEW VIRTUAL WHEN WHERE WITH WITHOUT))))

(for ([word (in-list sqlite-table-src)])
  (table-add! word '(sqlite)))

;; ================================================================================

(define hand-annotations
  '[
    ( avg                            sql92                     -function)
    ( bigint                               pgsql mysql         -type)
    ( bit                            sql92 pgsql               -type)
    ( bit_length                     sql92                     -function)
    ( blob                                       mysql         -type)
    ( boolean                              pgsql               -type)
    ( char                           sql92 pgsql mysql         -type)
    ( char_length                    sql92                     -function)
    ( character                      sql92 pgsql mysql         -type)
    ( character_length               sql92                     -function)
    ( coalesce                       sql92 pgsql               -function)
    ( convert                        sql92       mysql         -function)
    ( count                          sql92                     -function)
    ( current_catalog                      pgsql               -expr)
    ( current_date                   sql92 pgsql mysql sqlite  -expr)
    ( current_role                         pgsql               -expr)
    ( current_schema                       pgsql               -expr)
    ( current_time                   sql92 pgsql mysql sqlite  -expr)
    ( current_timestamp              sql92 pgsql mysql sqlite  -expr)
    ( current_user                   sql92 pgsql mysql         -expr)
    ( date                           sql92                     -type)
    ( decimal                        sql92 pgsql mysql         -type)
    ( double                         sql92       mysql         -type)
    ( false                          sql92 pgsql mysql         -expr)
    ( float                          sql92 pgsql mysql         -type)
    ( float4                                     mysql         -type)
    ( float8                                     mysql         -type)
    ( int                            sql92 pgsql mysql         -type)
    ( int1                                       mysql         -type)
    ( int2                                       mysql         -type)
    ( int3                                       mysql         -type)
    ( int4                                       mysql         -type)
    ( int8                                       mysql         -type)
    ( integer                        sql92 pgsql mysql         -type)
    ( interval                       sql92 pgsql mysql         -type)
    ( long                                       mysql         -type)
    ( longblob                                   mysql         -type)
    ( longtext                                   mysql         -type)
    ( lower                          sql92                     -function)
    ( max                            sql92                     -function)
    ( mediumblob                                 mysql         -type)
    ( mediumint                                  mysql         -type)
    ( mediumtext                                 mysql         -type)
    ( middleint                                  mysql         -type)
    ( min                            sql92                     -function)
    ( null                           sql92 pgsql mysql sqlite  -expr)
    ( nullif                         sql92 pgsql               -function)
    ( numeric                        sql92 pgsql mysql         -type)
    ( octet_length                   sql92                     -function)
    ( position                       sql92 pgsql               -function)
    ( smallint                       sql92 pgsql mysql         -type)
    ( substring                      sql92 pgsql               -function)
    ( sum                            sql92                     -function)
    ( time                           sql92 pgsql               -type)
    ( timestamp                      sql92 pgsql               -type)
    ( tinyblob                                   mysql         -type)
    ( tinyint                                    mysql         -type)
    ( tinytext                                   mysql         -type)
    ( trim                           sql92 pgsql               -function)
    ( true                           sql92 pgsql mysql         -expr)
    ( typeof                                           sqlite  -function)
    ( upper                          sql92                     -function)
    ( varbinary                                  mysql         -type)
    ( varchar                        sql92 pgsql mysql         -type)
    ( varcharacter                               mysql         -type)
    ( varying                        sql92       mysql         -type)
    ])

(for ([entry hand-annotations])
  (define word (symbol->string (car entry)))
  (define vals (set-intersect (cdr entry) '(-expr -function -type)))
  (table-add! word vals))

;; ================================================================================

(module+ main
  (require racket/date)
  (define dialects '(sql92 pgsql mysql sqlite -expr -function -type))
  (define h (make-hash))
  (for ([(word vals) (in-hash table)])
    (define vals* (set-intersect vals dialects))
    (unless (null? vals*)
      (hash-set! h word vals*)))
  (printf ";; Generated by sql-keywords.rkt at ~a\n\n" (date->string (current-date) #t))
  (write `(quote ,h))
  (newline))

#|
(define (write-definitions)
  (define dialects '(sql92 pgsql mysql sqlite))
  (for ([dialect dialects])
    (define reserved-words-var (string->symbol (format "~a-reserved-words" dialect)))
    (define reserved-words
      (sort (for/list ([(word ds) (in-hash table)] #:when (memq dialect ds)) word)
            string<?))
    (write `(define ,reserved-words-var ',reserved-words))
    (newline)
    (write `(provide ,reserved-words-var))
    (newline)))
|#
