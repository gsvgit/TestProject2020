import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import pandas as pd


labels = []
def add_label(violin, label):
    color = violin["bodies"][0].get_facecolor().flatten()
    labels.append((mpatches.Patch(color=color), label))

def draw(file, name, axs):
	df = pd.read_csv(file)
	data = [[d[1][0], d[1][1:]] for d in df.iterrows()]#[0::5]

	plt.ioff()

	r = [d[1] for d in data]
	lbl = [d[0]/1000 for d in data]
	
	add_label(axs.violinplot(r,
	   positions=lbl,
	   widths = 50,
	   showmeans=False,
	   showmedians=True),
 	   name)


def drawFiles (filesWithLegend, out):	
	fig = plt.figure()
	axs = plt.axes()

	axs.yaxis.grid(True)
	axs.set_xlabel('Длина входного списка (* 1000)')
	axs.set_ylabel('Время сортировки (миллисекунды)')

	for (file,legend) in filesWithLegend:
		draw(file, legend, axs)

	plt.legend(*zip(*labels), loc=2)

	plt.savefig(out)
	plt.close(fig)


drawFiles([('timingsCustomListSortOut.csv', "qSort"), 
	('timingsSystemListSortOut.csv', "List.sort")], 
	"ListSort5ReleaseGC.pdf")

labels = []
drawFiles([('timingsCustomListSortOut10_release.csv', "qSort"), 
	('timingsSystemListSortOut10_release.csv', "List.sort")], 
	"ListSort10ReleaseGC.pdf")

labels = []
drawFiles([('timingsCustomListSortOut5_debug.csv', "qSort"), 
	('timingsSystemListSortOut5_debug.csv', "List.sort")], 
	"ListSort5DebugGC.pdf")

labels = []
drawFiles([('timingsCustomListSortOut5_release_noGC.csv', "qSort"), 
	('timingsSystemListSortOut5_release_noGC.csv', "List.sort")], 
	"ListSort5ReleaseNoGC.pdf")

labels = []
drawFiles([('timingsCustomListSortOut5_release_GC_charging.csv', "qSort"), 
	('timingsSystemListSortOut5_release_GC_charging.csv', "List.sort")],
	"ListSort5ReleaseGCCharging.pdf")


labels = []
drawFiles([('timingsCustomListSortOut5_release_GC_charging.csv', "release, GC, charging"), 
	('timingsCustomListSortOut5_release_noGC.csv', "release, no GC, charging"), 
	('timingsCustomListSortOut5_debug.csv', "debug, GC, charging")], 
	"ListCustomSort5ReleaseGCCharging.pdf")


labels = []
drawFiles([('timingsCustomListSortOut5_release_GC_charging_2.csv', "release, GC, charging"),
    ('timingsCustomListSortOut5_release_noGC.csv', "release, no GC, no charging"), 
    ('timingsCustomListSortOut5_debug.csv', "debug, GC, charging")], 
    "ListCustomSort5ReleaseGCCharging2.pdf")


labels = []
drawFiles([('timingsSystemListSortOut5_release_GC_charging_2.csv', "release, GC, charging"), 
	('timingsSystemListSortOut5_release_noGC.csv', "release, no GC, no charging"), 
	('timingsSystemListSortOut5_debug.csv', "debug, GC, charging")],
	 "ListSystemSort5ReleaseGCCharging2.pdf")
