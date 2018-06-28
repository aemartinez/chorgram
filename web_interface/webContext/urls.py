from django.conf.urls import url
from webContext.views import index,get_file,upload_ajax,textarea_ajax,example,showdata_ajax,upload,input,get_file_return_dot,get_dot,template

urlpatterns = [
    url(r'^$',index,name='index'),
    url(r'^example/$',example,name='example'),
    url(r'^input/$',input,name="input"),
    url(r'^upload/$',upload,name='upload'),
    url(r'^get_file/$',get_file,name='get_file'),
    url(r'^get_dot/$',get_dot,name='get_dot'),
    url(r'^upload_ajax/$',upload_ajax,name='upload_ajax'),
    url(r'^textarea_ajax/$',textarea_ajax,name='textarea_ajax'),
    url(r'^showdata_ajax/$',showdata_ajax,name='showdata_ajax'),
    url(r'^get_file_return_dot/$',get_file_return_dot, name="get_file_return_dot"),
    url(r'^template/$',template,name='template'),
]