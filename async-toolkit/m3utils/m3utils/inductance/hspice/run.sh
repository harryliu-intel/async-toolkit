
# source ~/work/setup/hlp.setup

hspice test00.spice

/nfs/site/home/mnystroe/mst_work/m3utils/spice/ct/AMD64_LINUX/ct test00.tr0 out

aplot out
