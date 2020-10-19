import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import pandas as pd


labels = []
def add_label(violin, label):
    color = violin["bodies"][0].get_facecolor().flatten()
    labels.append((mpatches.Patch(color=color), label))

def draw(file, name):
	df = pd.read_csv(file)
	data = [[d[1][0], d[1][1:]] for d in df.iterrows()][0::5]

	plt.ioff()

	r = [d[1] for d in data]
	lbl = [d[0]/1000 for d in data]
	
	add_label(axs.violinplot(r,
	   positions=lbl,
	   widths = 50,
	   showmeans=False,
	   showmedians=True),
 	   name)

customListData = 'timingsCustomListSortOut.csv' 
systemListData = 'timingsSystemListSortOut.csv' 

fig = plt.figure()
axs = plt.axes()

axs.yaxis.grid(True)
axs.set_xlabel('Длинна входного списка (* 1000)')
axs.set_ylabel('Время сортировки (миллисекунды)')

draw(customListData,"qSort")

draw(systemListData, "List.sort")

plt.legend(*zip(*labels), loc=2)

plt.savefig("ListSort.pdf")
plt.close(fig)
