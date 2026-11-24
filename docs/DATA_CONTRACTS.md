# 当前数据契约

`feammatch2.theory.v2` 包含 Fields、Parameters、Lagrangian；EFT 另有 WilsonCoefficients。实标量采用 ScalarJet 表示普通导数，所有 Lorentz 标签成对收缩。每个 Wilson 系数线性乘以独立算符，参数总维数必须使每项作用量维数为四。

`CreateMatchingTask[uv,eft,request]` 返回 `feammatch2.matching-task.v2`，记录 UV、EFT、Request、LightFieldMap 和 Identity。请求仅接受 Loops、MaxDimension、PhysicalBasis、Scheme、Tadpole、Vacuum、ParameterWeights、LightFieldMap、PhysicalFieldMap、LoopSymbol。旧后端选择项和未知字段报错，旧冻结任务应重新创建。

`PlanMatching` 返回所有响应通道、动量行、顶点组合和工作阶数。它只接收当前 Task，Green 响应秩不足或不一致方程明确失败。

`ComputeTheoryAmplitudes[task,loop,directory,config]` 是 UV 低层阶段，loop 为 1 或 2。内部 `GenerationTheory` 用于标记 UV 反项顶点。没有 UV/EFT 选择参数或显式 EFT 图匹配入口。Region=AllHard 为记录信息，不是后端分派。

一圈结果保留 `OneLoopLaurentRules`、树级和辅助结果、UV 通道和 LocalPoleData。两圈保留 OneLoop、TwoLoopLaurentRules、EFTTwoLoopCounterterms、UVTreeCounterterms、DirectionRules、LocalDirectionData。`EFTTwoLoopCounterterms` 是轨迹上推断的反项，字段名不表示运行过 EFT 图。

`uv-one-loop-counterterms.wl` 含选定 tadpole 处方下实际用于插入的作用量和顶点，并保存应用处方前的势能极点分解。顶点系数采用每个拉氏量单项式的归一化。一圈与两圈步骤共用同一套 UV 反项。

`eq160-contributions.wl` 按离壳通道记录 `UVAllHard`、`UVOneLoopInsertionHard`、`EFTOneLoopCountertermDirection`。共享的 Process 记录用于最终投影的通道。三项均已按最终 Taylor 预算截断，仍保留 Laurent 极点，组合符号为加、加、减。`finite-direction-correction.wl` 保存带减号的第三项有限部分。

局域极点返回 Channels、DirectionChannels、Action、DirectionAction 和 GraphRecords。GraphRecords 指向实际 FeAmGen 图任务及振幅身份，由验收检查其 1PI 筛选及输出哈希。正式运行的 LocalPoleEvaluation 为 ExplicitOneLoop1PI。

源权重筛选产生 Skip=True、Graphs=0、Order=0 的决策记录；没有生成目录。此处 Order=0 是跳过标记，非一个生成阶数。

公开结果含 GreenRules、GreenLagrangian、Physical、Audit。Physical 中含 Rules、Lagrangian、KineticNormalization 和 WeightedFieldMaps。运行状态记录 `MatchingMethod=UVHardMSbar`、Green 基与物理基的检查结果及源码身份。`ExternalEFTGraphGeneration` 在一圈和两圈运行中为 `True`，树级运行中为 `False`。

所有 Failure 必须向上传播，不能转成零或 complete。真正零积分、空图集合和工具错误分开处理。

`performance.json` 使用 feammatch2.performance.v1，记录本次 RunMatching 的总墙钟时间、FORM 执行/命中计数及有起始偏移的阶段事件；嵌套阶段不能直接累加。它不替代 run-status 或科学验收。FORM 同次命中引用本次原始任务目录，标记 Cache=same_run_verified_hit 并保留输出哈希。
