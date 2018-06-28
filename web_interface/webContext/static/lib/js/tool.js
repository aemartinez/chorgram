/**
 * Created by liu on 2017/4/3.
 */
/**
 * fileinput plugin setting for upload page
 */
 $("#input-709").fileinput({
                        uploadUrl: /upload_ajax/, // server upload action
                        uploadAsync: true,
                        showPreview: false,
                        allowedFileExtensions: ['fsa', 'ssg'],
                        maxFileCount: 5,
                        elErrorContainer: '#kv-error-1'
                    }).on('filebatchpreupload', function(event, data, id, index) {
                        $('#kv-success-1').html('<h4>Upload Status</h4><ul></ul>').hide();
                    }).on('fileuploaded', function(event, data, id, index) {
                        var fname = data.files[index].name,
                            out = '<li>' + 'Uploaded file # ' + (index + 1) + ' - '  +
                                fname + ' successfully.' + '</li>';
                        $('#kv-success-1 ul').append(out);
                        $('#kv-success-1').fadeIn('slow');
                        file=fname;
                    });