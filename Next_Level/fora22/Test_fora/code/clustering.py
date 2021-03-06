import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import cluster_visualize as clu_viz

df = pd.read_csv('../seoul_data/DR_result.csv', encoding="EUC-KR")
df = df.iloc[:, 1:]
df_FA = df.iloc[:,0:4]
df_PCA = df.iloc[:,4:8]

from sklearn.preprocessing import StandardScaler

scaler = StandardScaler()
sl_FA = scaler.fit_transform(df_FA)
sl_PCA = scaler.fit_transform(df_PCA)

from sklearn.mixture import GaussianMixture
n=3
GMM = GaussianMixture(n_components=n, random_state=0)
gm_FA = GMM.fit_predict(sl_FA)
gm_PCA = GMM.fit_predict(sl_PCA)

# gm_viz_FA = pd.DataFrame(sl_FA) 
# gm_viz_FA['gm_FA_clusters'] = gm_FA
# gm_viz_FA.columns = ["ftr1", 'ftr2', 'ftr3', 'ftr4', 'gm_FA_clusters']
# clu_viz.visualize_cluster_plot(GMM,gm_viz_FA, 'gm_FA_clusters', iscenter = False )

# gm_viz_PCA = pd.DataFrame(sl_PCA) 
# gm_viz_PCA['gm_PCA_clusters'] = gm_PCA
# gm_viz_PCA.columns = ["ftr1", 'ftr2', 'ftr3', 'ftr4', 'gm_PCA_clusters']
# clu_viz.visualize_cluster_plot(GMM,gm_viz_PCA, 'gm_PCA_clusters', iscenter = False )

from sklearn.cluster import MeanShift, estimate_bandwidth
from itertools import cycle

def get_ms(scaled_data):
    bandwidth=estimate_bandwidth(scaled_data)
    meanshift=MeanShift(bandwidth=bandwidth)
    labels=meanshift.fit_predict(scaled_data)
    lab = meanshift.labels_
    print(lab == labels)
    cluster_centers = meanshift.cluster_centers_
    labels_unique = np.unique(lab)
    n_clusters_ = len(labels_unique)

    plt.figure(1)
    plt.clf()

    colors = cycle('bgrcmykbgrcmykbgrcmykbgrcmyk')
    for k, col in zip(range(n_clusters_), colors):
        my_members = lab == k
        cluster_center = cluster_centers[k]
        plt.plot(scaled_data[my_members, 0], scaled_data[my_members, 1], col + '.')
        plt.plot(cluster_center[0], cluster_center[1], 'o', markerfacecolor=col,
                markeredgecolor='k', markersize=14)
    plt.title('Estimated number of clusters: %d' % n_clusters_)
    plt.show()

    return labels

ms_FA = get_ms(sl_FA)
ms_PCA = get_ms(sl_PCA)

data = {
    'gm_FA' : gm_FA,
    'ms_FA' : ms_FA,
    'gm_PCA' : gm_PCA,
    "ms_PCA" : ms_PCA
}
gm_ms = pd.DataFrame(data)
gm_ms.to_csv('../seoul_data/gm_ms.csv', encoding = 'utf-8')