#Lucas Manker
#10/15/19
import mysql.connector
import subprocess as sub

prof_name = ''


def calc_years_active(link):
    connection = mysql.connector.connect(
        host="uwyobibliometrics.hopto.org",
        database="bibliometrics",
        user="luke",
        password="K8H,3Cuq]?HzG*W7",
        auth_plugin="mysql_native_password"
    )
    cursor = connection.cursor(prepared=True)
    sql_query = "select year from citations where link = %s group by title"
    sql_input = (link,)
    cursor.execute(sql_query, sql_input)
    results = cursor.fetchall()
    year_arr = []
    pub_count = []
    cit_count = []
    h_index = []
    for x in results:
        if x[0] not in year_arr:
            year_arr.append(x[0])
    for x in year_arr:
        sql_query = "select count(distinct title) from citations where pub_date = %s and link =%s"
        sql_input = (x, link)
        cursor.execute(sql_query, sql_input)
        results = cursor.fetchall()
        pub_count.append(results[0][0])
    for x in year_arr:
        sql_query = "select sum(count) from citations where pub_date <= %s and link =%s"
        sql_input = (x, link)
        cursor.execute(sql_query, sql_input)
        results = cursor.fetchall()
        cit_count.append(results[0][0])
    for x in range(len(year_arr)):
        pub_count[x] = (year_arr[x], pub_count[x], cit_count[x])
    cit_sum = 0
    pub_sum = 0
    for x in pub_count:
        cit_sum += int(x[2])
        pub_sum += x[1]
        if cit_sum >= pub_sum:
            h_index.append((x[0], pub_sum))
    with open('h.csv', 'w') as f:
        f.write("Year,H \n")
        for x in h_index:
            f.write(str(x[0]) + "," + str(x[1]) + "\n")
    sql_query = "select author from profiles where link = %s"
    sql_input = (link,)
    cursor.execute(sql_query, sql_input)
    results = cursor.fetchall()
    return results[0][0]


prof_name = calc_years_active("ZH31nVgAAAAJ")
sub.call(['C:/Program Files/R/R-3.6.0/bin/Rscript',  '--vanilla', 'C:/Users/Luke/Desktop/parser/indvGraph.r',
          prof_name])
