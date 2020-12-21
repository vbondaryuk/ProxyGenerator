using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using NUnit.Framework;

namespace ProxyGenerator.Tests
{
    [ProxyObjectGenerator.Proxy]
    public interface IInterface : IInterface1
    {
        void QWE();
        Task<int> Q();
        int Q1(int[] qwe, decimal q, List<IInterface> interfaces);
    }

    public interface IInterface1
    {
        Task I2();
        int I12(int[] qwe, decimal q, Action<IInterface> interfaces);
    }

    public class Tests
    {
        [SetUp]
        public void Setup()
        {
            
        }

        [Test]
        public async Task Test1()
        {
            IInterface interfaceProxy = new IInterfaceProxy();
            int i = await interfaceProxy.Q();
            interfaceProxy.QWE();
            await interfaceProxy.I2();

        }
    }
}