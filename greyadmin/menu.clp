<ga_with-wrapper>
<ga_user-address/><br>
<wa_showerrors name="error" session clear/>
<br>
<b>Current settings</b><br>
<form action="update" method="post">
Greylisting: 
<input type=radio name=greylisting value="on" 
 <clp_ifdef name="greylisting" session>checked</clp_ifdef>> 
On
<input type=radio name=greylisting value="off"
 <clp_ifndef name="greylisting" session>checked</clp_ifndef>>
Off
<br>
<br>
<input type=submit value="Update settings"><br>
</form>

<hr>
<ga_list-delayed-triples/>

<form action="menu">
<input type=submit value="Refresh"><br>
</form>


</ga_with-wrapper>
