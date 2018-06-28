from django import forms

class ContactForm(forms.Form):
    name = forms.TimeField
    message = forms.CharField(widget=forms.Textarea)

    def send_fsa(self):
        pass