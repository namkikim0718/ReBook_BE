package com.be.rebook.product.repository;

import com.be.rebook.product.entity.Product;
import com.be.rebook.product.entity.ProductImage;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface ProductImageRepository extends JpaRepository<ProductImage, Long> {
    List<ProductImage> findByProduct(Product product);
}
