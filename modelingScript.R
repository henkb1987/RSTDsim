new.path <- GenerateInputFile("henk.jif",data.frame(key = "@A",new.value = 0,stringsAsFactors = F))
new.path <- bat.maker(gsub("\\.jif","",new.path),n = 10)
shell(paste0("C:\\Users\\manZ130194\\Documents\\STDsim\\",new.path))
outputParser()