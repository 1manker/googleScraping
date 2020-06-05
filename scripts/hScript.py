import mysql.connector
import math

def create_queue():
	connection = mysql.connector.connect(
         host="uwyobibliometrics.hopto.org",
         database="bibliometrics",
         user="luke",
         password="K8H,3Cuq]?HzG*W7",
         auth_plugin="mysql_native_password"
        )
	sql_query = "select link from profiles where queue_status = true"
	cursor = connection.cursor(prepared=True)
	cursor.execute(sql_query)
	results = cursor.fetchall()
	f = open("queue.txt", "w")
	for result in results:
		f.write(result[0].decode() + ",")
	f.close()


def calc(profile):
	connection = mysql.connector.connect(
         host="uwyobibliometrics.localhost",
         database="bibliometrics",
         user="luke",
         password="K8H,3Cuq]?HzG*W7",
         auth_plugin="mysql_native_password"
        )
	sql_query = "select min(year), max(year) from citations where link = %s"
	cursor = connection.cursor(prepared=True)
	link = (profile,)
	cursor.execute(sql_query, link)
	results = cursor.fetchall()
	cursor.close()
	if(results[0][0] == None or results[0][1] == None):
		return
	for x in range(results[0][0], results[0][1]):
		print(x)
		sql_query = "select distinct(title), sum(count) from citations where link = %s and year <= %s group by title;"
		cursor = connection.cursor(prepared=True)
		sql_input = (profile, str(x))
		cursor.execute(sql_query, sql_input)
		res = cursor.fetchall()
		cursor.close()
		h = 1
		counts = []
		for k in res:
			counts.append(int(k[1].decode()))
		while(True):
			if(len([i for i in counts if i > h])>h):
				h += 1
			else:
				print("H-index: ",h)
				break
		i10 = len([i for i in counts if i > 10])
		print("i10 index:", i10)
		counts.sort()
		counts.reverse()
		g = 1
		while(True):
			if(g >= len(counts)):
				print("g-index: ", g)
				break
			elif(sum(counts[0:g])>g*g):
				g += 1
			else:
				print("g-index: ", g)
				break
		sql_add = "insert into metrics values(%s,%s,%s,%s,NOW(),%s)"
		cursor = connection.cursor(prepared=True)
		sql_input = (profile, str(h), str(i10), str(g), x)
		cursor.execute(sql_add, sql_input)
		connection.commit()
		cursor.close()

def process_queue():
	f = open("queue.txt", "r")
	profiles = f.read()
	profiles = profiles.split(",")
	for profile in profiles:
		calc(profile)
	f.close()


create_queue()
