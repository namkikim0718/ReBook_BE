package com.be.rebook.product.repository;

import com.be.rebook.product.entity.Product;

import java.util.List;

public interface ProductRepositoryCustom {

    List<Product> findProductsByFilter(String university, String major, String title, Integer minPrice, Integer maxPrice, String order);
}
