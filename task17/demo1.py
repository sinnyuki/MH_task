import csv

source_file_path = "C:/Users/hmz19/Desktop/demo/table1.csv"   #输入的csv表格路径路径
res_file_path = "C:/Users/hmz19/Desktop/demo/res.csv"         #保存结果的csv表格文件路径


file = open(source_file_path)
reader = csv.reader(file)
header_row = next(reader)   #得到标题行
line_num = len(header_row)  #计算列数
row_num = 0
line_l = [[] for i in range(line_num)] #防止浅拷贝
for row in reader:
    row_num = row_num + 1
    for i in range(0,line_num):
        line_l[i].append(row[i])
print("get ok")

res_line_l = []
for i in range(line_num):
    res_line_l.append([])
    for j in range(line_num):
        res_line_l[i].append("")

for o_cnt in range(line_num):
    if(o_cnt < (line_num - 1)):
        for b_cnt in range(o_cnt + 1, line_num):
            res_num = 0
            for row_cnt in range(row_num):
                if((line_l[o_cnt][row_cnt] == "1") & (line_l[b_cnt][row_cnt] == "1")):
                    res_num = res_num + 1
            res_line_l[o_cnt][b_cnt] = str(res_num)
for i in range(line_num):
    s_str = "s" + str(i + 1)
    res_line_l[i].insert(0, s_str)
res_line_l.insert(0, []) #插入标题行
for i in range(line_num + 1):
    if(i > 0):
        s_str = "s" + str(i)
        res_line_l[0].append(s_str)
    else:
        res_line_l[0].append("")


file_w = open(res_file_path, "w+", encoding="utf-8")
res_writer = csv.writer(file_w)
for a in res_line_l:
    print(a)
    res_writer.writerow(a)
file.close()
file_w.close()
