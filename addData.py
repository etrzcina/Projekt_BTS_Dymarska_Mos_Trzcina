import psycopg2
import os

connection=dbname='tele' user='user' host='localhost' password='user'
conn = psycopg2.connect(connection)
cursor = conn.cursor()

def readFiles(path)
   with open(path, r) as ins
       array=[]
       for line in ins
           if (line.find('&amp;')!=-1 or line.find('&#39;')!=-1 or line.find('&quot;')!=-1)
               line = line.replace(&amp;,&)
               line = line.replace(&#39;,)
               line = line.replace(&quot;,)
           fields=line.strip().split(';')
           array.append(fields)

   array.remove(array[0])
   for item in array

       mcc_mnc = int(item[0])
       cell_id = int(item[1])
       lac = int(item[2])
       rnc= int(item[3])
       lat= float(item[4])
       lon= float(item[5])
       ratio= int(item[6])
       cell_data= item[7]
       rfu= int(item[8])

       query =  INSERT INTO transmitters VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s);
       data = (mcc_mnc, cell_id, lac, rnc, lat, lon, ratio, cell_data, rfu)
       cursor.execute(query, data)
       conn.commit()
   return

listOfFiles=os.listdir('mediasf_osgeolive-vm-9.0transmitters')
for i in listOfFiles
   readFiles(os.path.abspath('mediasf_osgeolive-vm-9.0transmitters'+i))
