### Objective
***
- task is to calculate MRR of a plugin based on atlassian addon sales and licenses data				
- NOTE on redacted info :			
  - only the privacy info were removed from the data		
- such as `company info, emails, people names` as they are not necesary for MRR calculation		
- transaction ids, license ids, etc - were changed to appropriztely same unique value - to not to preresent real world cases due to privacy		
- sales transactions - as per			
https://developer.atlassian.com/platform/marketplace/rest/#api-vendors-vendorId-reporting-sales-transactions-export-get		
  - all records without filters		
- license - as per			
https://developer.atlassian.com/platform/marketplace/rest/#api-vendors-vendorId-reporting-licenses-export-get
  - less evaluations and community licenses as they have 0 value and are no relevant for MRR