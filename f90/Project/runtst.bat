del copy.dat
del aw-resI
del aw-resZ
del aw-res
copy runstreams\G%1.DDD copy.dat
copy manage.z manage.dat
f2-gup2 >a.0
copy manage.i manage.dat
f2-gup2 >a.0
copy manage.%2 manage.dat
f2-gup2 >a.1
aw-res7 >a.2
proc > ..\ALLOUT\figout%2.%1
copy aw-traj.out ..\ALLOUT\aw-traj%2.%1
copy aw-conf.out ..\ALLOUT\aw-conf%2.%1
copy aw-sum.out ..\ALLOUT\aw-sum%2.%1
#copy aw-resZ ..\ALLOUT\aw-resZ%2.%1
#del aw-resI
#del aw-resZ
#del aw-res
del aw-traj.out
del aw-sum.out
del *.20
del *.100
del *.res
del aw-table
del aw-1-2.out
del aw-conf.out

