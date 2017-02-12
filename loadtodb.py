from pymongo import MongoClient

client = MongoClient("localhost:27017")
db = client.Appenddata
rootdir='D:/2016-09/2016/09'


for subdir,dirs,files in os.walk(rootdir):
    for file in files:
        f=gzip.open('%s\%s'%(subdir,file),'r')
        file_content = f.read().splitlines()
        f.close()
        print(file,f,len(file_content))
        
        for i in range(0,len(file_content)):
            result = db.table1.insert(json.loads(file_content[i].decode('UTF-8')),check_keys = False)

