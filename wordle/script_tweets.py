import twint
 
c = twint.Config()
c.Search = "wordle '/6'"
# PARA EL WORDLE EN ESPAÑOL:
# c.Search = "'Wordle (ES)' '/6'"
# PARA EL WORDLE EN CATALÁN:
# c.Search = "#WordleCAT"
c.Store_csv = True
c.Output = "wordle.csv"
 
twint.run.Search(c)
