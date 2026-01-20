#!/bin/bash
#SBATCH --job-name=cholera_wp
#SBATCH --output=slurm/logs/cholera_wp_%A_%a.out
#SBATCH --error=slurm/logs/cholera_wp_%A_%a.err
#SBATCH --time=24:00:00
#SBATCH --mem=16gb
#SBATCH --ntasks=4
#SBATCH --nodes=1
#SBATCH --array=0-1

# Rerun weightprior gt=5 inc=4 split into TRUE and FALSE
# Each run separately to avoid 24h timeout

if [[ "$SLURM_SUBMIT_DIR" == */slurm ]]; then
    cd "$SLURM_SUBMIT_DIR/.." || exit 1
else
    cd "$SLURM_SUBMIT_DIR" || exit 1
fi

module load R

GT=5
INC=4
RT_OPTS="latest"
DISEASE="cholera"

case $SLURM_ARRAY_TASK_ID in
    0) # weight_prior=TRUE only
        echo "Running: weightprior TRUE gt=$GT inc=$INC"
        Rscript -e "
        library(here)
        source(here('scripts', '01_packages.R'))
        source(here('scripts', '02b_definedelays.R'))
        source(here('scripts', 'datacollect_casestudy.R'))
        source(here('R', 'funcs_data.R'))
        source(here('R', 'scenario_loop.R'))

        d <- delays[['$DISEASE']]
        case_data <- casestudydata[['$DISEASE']]

        res <- sim_weightprior(case_data=case_data,
                               var=$GT, inc=$INC,
                               gen_mean_mean=d\$gen[['mean']],
                               gen_mean_sd=d\$gen[['mean_sd']],
                               gen_sd_mean=d\$gen[['sd']],
                               gen_sd_sd=d\$gen[['sd_sd']],
                               gen_max=d\$gen[['max']],
                               inc_mean_mean=d\$inc[['mean']],
                               inc_mean_sd=d\$inc[['mean_sd']],
                               inc_sd_mean=d\$inc[['sd']],
                               inc_sd_sd=d\$inc[['sd_sd']],
                               inc_max=d\$inc[['max']],
                               rep_mean_mean=d\$rep[['mean']],
                               rep_mean_sd=d\$rep[['mean_sd']],
                               rep_sd_mean=d\$rep[['sd']],
                               rep_sd_sd=d\$rep[['sd_sd']],
                               rep_max=d\$rep[['max']],
                               freq_fc=4, weeks_inc=12,
                               rt_opts_choice='latest',
                               weight_prior=TRUE,
                               obs_scale=d\$underreport)

        save_latest(res[[1]], here('results'), paste0('res_${DISEASE}_weightprior_TRUE${RT_OPTS}_samples', $GT, $INC))
        save_latest(res[[2]], here('results'), paste0('res_${DISEASE}_weightprior_TRUE${RT_OPTS}_id', $GT, $INC))
        save_latest(res[[3]], here('results'), paste0('res_${DISEASE}_weightprior_TRUE${RT_OPTS}_R', $GT, $INC))
        save_latest(res[[4]], here('results'), paste0('res_${DISEASE}_weightprior_TRUE${RT_OPTS}_summary', $GT, $INC))
        save_latest(res[[5]], here('results'), paste0('res_${DISEASE}_weightprior_TRUE${RT_OPTS}_warnings', $GT, $INC))
        save_latest(res[[6]], here('results'), paste0('res_${DISEASE}_weightprior_TRUE${RT_OPTS}_timing', $GT, $INC))
        "
        ;;
    1) # weight_prior=FALSE only
        echo "Running: weightprior FALSE gt=$GT inc=$INC"
        Rscript -e "
        library(here)
        source(here('scripts', '01_packages.R'))
        source(here('scripts', '02b_definedelays.R'))
        source(here('scripts', 'datacollect_casestudy.R'))
        source(here('R', 'funcs_data.R'))
        source(here('R', 'scenario_loop.R'))

        d <- delays[['$DISEASE']]
        case_data <- casestudydata[['$DISEASE']]

        res <- sim_weightprior(case_data=case_data,
                               var=$GT, inc=$INC,
                               gen_mean_mean=d\$gen[['mean']],
                               gen_mean_sd=d\$gen[['mean_sd']],
                               gen_sd_mean=d\$gen[['sd']],
                               gen_sd_sd=d\$gen[['sd_sd']],
                               gen_max=d\$gen[['max']],
                               inc_mean_mean=d\$inc[['mean']],
                               inc_mean_sd=d\$inc[['mean_sd']],
                               inc_sd_mean=d\$inc[['sd']],
                               inc_sd_sd=d\$inc[['sd_sd']],
                               inc_max=d\$inc[['max']],
                               rep_mean_mean=d\$rep[['mean']],
                               rep_mean_sd=d\$rep[['mean_sd']],
                               rep_sd_mean=d\$rep[['sd']],
                               rep_sd_sd=d\$rep[['sd_sd']],
                               rep_max=d\$rep[['max']],
                               freq_fc=4, weeks_inc=12,
                               rt_opts_choice='latest',
                               weight_prior=FALSE,
                               obs_scale=d\$underreport)

        save_latest(res[[1]], here('results'), paste0('res_${DISEASE}_weightprior_FALSE${RT_OPTS}_samples', $GT, $INC))
        save_latest(res[[2]], here('results'), paste0('res_${DISEASE}_weightprior_FALSE${RT_OPTS}_id', $GT, $INC))
        save_latest(res[[3]], here('results'), paste0('res_${DISEASE}_weightprior_FALSE${RT_OPTS}_R', $GT, $INC))
        save_latest(res[[4]], here('results'), paste0('res_${DISEASE}_weightprior_FALSE${RT_OPTS}_summary', $GT, $INC))
        save_latest(res[[5]], here('results'), paste0('res_${DISEASE}_weightprior_FALSE${RT_OPTS}_warnings', $GT, $INC))
        save_latest(res[[6]], here('results'), paste0('res_${DISEASE}_weightprior_FALSE${RT_OPTS}_timing', $GT, $INC))
        "
        ;;
esac

echo "Done: task=$SLURM_ARRAY_TASK_ID"
