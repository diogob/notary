
var getCurrencies = function()
{
  return axios({ url: '/currencies'
    , method: 'get'
    });
}
