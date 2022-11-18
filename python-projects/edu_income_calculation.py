import pandas as pd
import numpy as np
df = pd.read_excel('JointSurvey2_cleaned_new.xlsx')

q30 = list(filter(lambda x: 'q30' in x, df.columns))
dff = df[['region', 'incomelevelname','coun']+q30]
dff['incomegroup'] = dff.incomelevelname.fillna('No response')
dff['region'] = dff.region.fillna('No response')

def colap(x,flag):
	x = list(x)
	if(flag==1):
		if('Yes' in x):
			return 1
		elif(('Yes' not in x) and ('No' in x)):
			return 0
		elif(('Yes' not in x) and ('No' not in x) and ('Do not know' in x)):
			return 2
		else:
			return 3
	else:
		if('Yes' in x):
			return 1
		elif(('Yes' not in x) and (('No' in x) or ('Do not know' in x))):
			return 0
		else:
			return 2			



dff['q30_p_sep'] = dff[list(filter(lambda x: 'q30p' in x, df.columns))].apply(lambda x: colap(x,1),axis=1)
dff['q30_ls_sep'] = dff[list(filter(lambda x: 'q30ls' in x, df.columns))].apply(lambda x: colap(x,1),axis=1)
dff['q30_us_sep'] = dff[list(filter(lambda x: 'q30us' in x, df.columns))].apply(lambda x: colap(x,1),axis=1)


dff['q30_p_tog'] = dff[list(filter(lambda x: 'q30p' in x, df.columns))].apply(lambda x: colap(x,0),axis=1)
dff['q30_ls_tog'] = dff[list(filter(lambda x: 'q30ls' in x, df.columns))].apply(lambda x: colap(x,0),axis=1)
dff['q30_us_tog'] = dff[list(filter(lambda x: 'q30us' in x, df.columns))].apply(lambda x: colap(x,0),axis=1)

sepcol = list(filter(lambda x: '_sep' in x, dff.columns))
togcol = list(filter(lambda x: '_tog' in x, dff.columns))


q30e_sep_df = pd.DataFrame()
for k in ['region', 'incomelevelname']:
	for j in sepcol:
		tmp1 = dff.groupby([k,j]).coun.count().reset_index()
		tmp1['question'] = j.strip().split('_')[1]
		tmp1.columns  = ['group_name','question_response','group_count','question']
		tmp2 = dff.groupby(k).coun.count().reset_index()
		tmp2.columns  = ['group_name','group_total']
		tmpdf = tmp1.merge(tmp2,on='group_name',how='inner')
		tmpdf['percent'] = (tmpdf['group_count']*100)/tmpdf['group_total']
		q30e_sep_df  = q30e_sep_df.append(tmpdf)

final_df_sep = pd.pivot_table(q30e_sep_df, values=['percent'], index=['group_name', 'question_response'],columns=['question']).reset_index()



q30e_tog_df = pd.DataFrame()
for k in ['region', 'incomelevelname']:
	for j in togcol:
		tmp1 = dff.groupby([k,j]).coun.count().reset_index()
		tmp1['question'] = j.strip().split('_')[1]
		tmp1.columns  = ['group_name','question_response','group_count','question']
		tmp2 = dff.groupby(k).coun.count().reset_index()
		tmp2.columns  = ['group_name','group_total']
		tmpdf = tmp1.merge(tmp2,on='group_name',how='inner')
		tmpdf['percent'] = (tmpdf['group_count']*100)/tmpdf['group_total']
		q30e_tog_df  = q30e_tog_df.append(tmpdf)

final_df_tog = pd.pivot_table(q30e_tog_df, values=['percent'], index=['group_name', 'question_response'],columns=['question']).reset_index()




writer = pd.ExcelWriter('q30_edu_percent_new.xlsx', engine = 'xlsxwriter')
final_df_sep.to_excel(writer, sheet_name = 'no_and_dontknow_sep')
final_df_tog.to_excel(writer, sheet_name = 'no_and_dontknow_together')
q30e_sep_df.to_excel(writer, sheet_name = 'raw_data_no_and_dontknow_sep')
q30e_tog_df.to_excel(writer, sheet_name = 'raw_data_no_and_dontknow_tog')
writer.save()
writer.close()
