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
<b>Statistics</b><br>
<ul>
<li>Number of delivery attempts that have been delayed: 
 <ga_num-blocked-triples/></li>
<ul>
    <li>Of those, the number that have never been accepted (likely spams): <ga_num-suspected-spams/></li>
</ul>
<li>Number of delivery attempts that have been accepted:
 <ga_num-passed-triples/></li>
</ul>
</ga_with-wrapper>
