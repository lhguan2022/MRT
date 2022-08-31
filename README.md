## 資料
- 臺北捷運各站每日分時運量，期間從2017年1月至2022年七月
- 註：2021年以前之資料中只有108個捷運站，自2021年增設環狀線後，捷運站數量增加為119個

## 說明
1. 檔案 `MRT_data_preprocess.R` 將原始資料轉換為 `result_2017.csv` $\cdots$ `result_2022.csv`
2. 檔案 `MRT_index.R` 將 `result_2017.csv` $\cdots$ `result_2022.csv` 轉換為 `index_2017.csv` $\cdots$ `index_2022.csv`
3. 對於捷運站 $A$，我們有如下表格

| Hour     | Value_i    | Value_o    |
|----------|------------|------------|
| $00$     | $v_0$      | $u_0$      |
| $01$     | $v_1$      | $u_1$      |
| $05$     | $v_5$      | $u_5$      |
| $\vdots$ | $\vdots$   | $\vdots$   |
| $23$     | $v_{23}$   | $u_{23}$   |

則各別index計算方式如下:

$$index_{1} = v_5 + \cdots + v_{12}$$

$$index_{2} = \frac{\max(v_5, \cdots, v_{12})}{\max(u_5, \cdots, u_{12})}$$

$$index_{3} = \frac{v_5 + \cdots + v_{12}}{u_5 + \cdots + u_{12}}$$

$$index_{4} = \frac{v_5 + \cdots + v_{12}}{v_5 + \cdots + v_{12} + u_5 + \cdots + u_{12}}$$
