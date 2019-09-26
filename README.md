Simple Scala CLI application.

User's avaiable actions:  
ARRIVE $weight - appends to documents' gate queue new truck with given $weight  
STEP           - advances the state of whole system in a one time interval  
STATUS         - prints the state of whole system in a currnet time interval  
AVGTIME        - prints average time waiting for trucks between docuemnts' and cargos' gates  

Example Usage  
ARRIVE 4  
ARRIVE 6  
ARRIVE 7  
ARRIVE 2  
ARRIVE 5  
STATUS  
STEP  
STEP  
STATUS  
STEP  
STATUS  
AVGTIME  
