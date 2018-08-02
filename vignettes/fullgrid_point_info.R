library(rgeos)
library(horizon)
cfg<-config::get(file="config/config.yml")

fname = cfg$svf_fullgrid
st<- stack(fname,bands=1)

skyviewf<-svf(st,
              blockSize=40,
              ll=FALSE,
              nAngles=16,
              maxDist=200,
              filename="/nobackup/users/dirksen/data/SVF/5m_grid/svf_200m_16d.grd")
