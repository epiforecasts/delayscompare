## Task list for the dense-origin turning-point fits (slurm/02_run_turningpoint_dense.sh).
## Step trajectories: origins every 3 days from 14 days before to 34 days after the
## change on day 105. Disease-like replicates 1-10: weekly origins between the
## existing four-weekly ones (days 35-217, skipping multiples of 28), which together
## with them give a weekly grid. Both delay axes: generation interval levels 1-6 with
## the correct observation delay, and observation delay levels 1-3, 5, 6 with the
## correct generation interval.
library(here)
cells <- rbind(data.frame(gt = 1:6, inc = 4), data.frame(gt = 4, inc = c(1:3, 5:6)))
step_days <- 105 + seq(-14, 34, by = 3)
step <- expand.grid(day = step_days, rep = 1:3, tag = c("bend", "turn", "bendup", "turnup"),
                    disease = c("ebola", "covid", "cholera"), stringsAsFactors = FALSE)
resim_days <- setdiff(seq(35, 217, by = 7), seq(28, 224, by = 28))
resim <- expand.grid(day = resim_days, rep = 1:10, tag = "-",
                     disease = c("ebola", "covid", "cholera"), stringsAsFactors = FALSE)
origins <- rbind(step, resim)
tasks <- merge(origins, cells)[, c("disease", "tag", "rep", "day", "gt", "inc")]
tasks <- tasks[order(tasks$tag == "-", tasks$disease, tasks$tag, tasks$rep, tasks$day, tasks$gt, tasks$inc), ]
write.table(tasks, here("slurm", "tasks_turningpoint_dense.txt"), quote = FALSE,
            row.names = FALSE, col.names = FALSE)
message(nrow(tasks), " tasks: ", sum(tasks$tag != "-"), " step, ", sum(tasks$tag == "-"), " disease-like")
