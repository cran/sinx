这个问题我很感兴趣。能否提供一个可以重现问题的数据和代码示例，让我直接拷贝粘贴就能运行？最好附带 `session_info()` 信息。

提问题的方式可以参考“[新手须知 1.2 正确的发帖姿势](https://d.cosx.org/d/1553)”。

author: dapengde

context: <https://d.cosx.org/d/420469/2>

source: d.cosx.org

date: 2018-12

---

奇怪了你的代码在我的机器上完全没有问题啊

author: tctcab

context: <https://d.cosx.org/d/7673/15>

source: d.cosx.org

date: 2018-09

---

节约别人的时间，就是节约自己的时间。

举个例子，您是从医学的观点出发进行数据分析，对统计以及数理细节其实并不关心，属于“理解当然更好，但是没有也无所谓”的情况。而您真正关心的是，哪个r包，哪个算法，或者跟确切一点，哪个语句能给出您所期待的答案。



为什么说会节约时间呢，很简单的一个问题，比如说对我来说，要复现你的问题，那首先得：



1. 找一个p>n 的数据集

2. 研究一下glmnet怎么用

3. 再写一个3~10行的脚本跑一下看看结果




虽然1，2，3都是很简单的很容易做到的工作，但是很麻烦，这又不是我的工作内容。虽然培训班网上教程都讲烂了，但是不好意思我没空去看那些。包的手册虽然也能看没问题，但是还是很麻烦。那既然那么麻烦，就还是算了吧。那把我知道的东西告诉你，能帮多少算多少，剩下的就只有祝你好运了。

但是如果贴上来的话，我就不用去做1，2，3了，那直接解决4还是一个比较有意思的活。虽然不能保证能解决，但是至少在我有空的时候（比如昨天）可以尝试尝试，如果做到了，那直接可以把代码给你，互相都能省很多时间。

而且惰性是人类固有的东西，就比如打以上这段话的时间都够把网上培训班教程再看两遍再写一个脚本了，但是我还是懒得去做这些麻烦的事情而选择在这里慢慢打字，因为这样不费脑子，至少不费管理科的脑子。。。。。

author: wglaive

context: <https://d.cosx.org/d/420212/6>

source: d.cosx.org

date: 2018-10

---

Sweave 好像就是一边编译 LaTeX 的时候就可以一边执行程序源代码并即时输出结果，我也没正式用过……

author: yihui

context: <https://d.cosx.org/d/420198/2>

source: d.cosx.org

date: 2007-10

---

如果你去看医生，去医院了对医生说，我生病了，你别管我生了什么病，马上给我开药吧。

医生能开么？另：Mac和Unix怎么扯上关系了？……

author: yihui

context: <https://d.cosx.org/d/9234-9234/3>

source: d.cosx.org

date: 2007-12

---

那我就不知道为什么了……

author: yihui

context: <https://d.cosx.org/d/420428/4>

source: d.cosx.org

date: 2019-01

---

你的 TeX 用的是什么发行版？中文模板需要很多额外的包。如果你装的是 tinytex 包和 TinyTeX 发行版，那就好办了，缺的包会自动装。如果你装的是别的发行版，例如 MiKTeX 或者 TeX Live 等，可能要手动安装一些包。发行版安装一个就够，不要同时安装多个。

最后，如果还不行，可以试试 bookdown 的[官方中文模板](https://github.com/yihui/bookdown-chinese)。如果好用，那再考虑解决 bookdownplus 里的模板问题；否则就说明仍然是 bookdown 的环境没弄好，请运行下面的代码，看看结果是什么：

```
tinytex:::is_tinytex()
tinytex:::tinytex_root()
tinytex::tlmgr_update()
Sys.which('pdflatex')
system2('pdflatex', '--version')
```

或者看这里：<<https://yihui.name/tinytex/r/#debugging>>

author: dapengde

context: <https://d.cosx.org/d/419258>

source: d.cosx.org

date: 2019-02

---

看了你最近提出的一系列问题，我觉得大部分属于 R 语言的基本操作。只要通读一下基础的入门书，这些问题都会迎刃而解。否则，即使别人临时帮你解决了这些问题，后续仍然会问题不断。

祝你好运。

author: dapengde

context: <https://d.cosx.org/d/420485/2>

source: d.cosx.org

date: 2019-02

---

八字箴言：遇到问题，先试升级。

author: yihui

context: https://d.cosx.org/d/419135/23

source: d.cosx.org

date: 2016-06-09
