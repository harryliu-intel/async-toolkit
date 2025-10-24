FORFILES="mul_mtransposem_sp.f mul_mtransposem_dp.f mulmv_sp.f mulmv_dp.f lu2_backsubstitute_sp.f lu2_backsubstitute_dp.f indexeddot_sp.f indexeddot_dp.f delta_sp.f delta_dp.f "
for f in ${FORFILES}; do
  echo ${f}:
  ifort -msse2 -axTPSW -S ${f}
done
